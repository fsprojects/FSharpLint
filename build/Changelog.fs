module Changelog

open System
open Fake.Core
open Fake.IO

let isEmptyChange =
    function
    | Changelog.Change.Added s
    | Changelog.Change.Changed s
    | Changelog.Change.Deprecated s
    | Changelog.Change.Fixed s
    | Changelog.Change.Removed s
    | Changelog.Change.Security s
    | Changelog.Change.Custom (_, s) -> String.IsNullOrWhiteSpace s.CleanedText

let tagFromVersionNumber versionNumber = sprintf "v%s" versionNumber

let failOnEmptyChangelog (latestEntry : Changelog.ChangelogEntry) =
    let isEmpty =
        (latestEntry.Changes |> Seq.forall isEmptyChange)
        || latestEntry.Changes |> Seq.isEmpty

    if isEmpty then
        failwith "No changes in CHANGELOG. Please add your changes under a heading specified in https://keepachangelog.com/"

let mkLinkReference (newVersion : SemVerInfo) (changelog : Changelog.Changelog) (gitHubRepoUrl : string) =
    if changelog.Entries |> List.isEmpty then
        // No actual changelog entries yet: link reference will just point to the Git tag
        sprintf "[%s]: %s/releases/tag/%s" newVersion.AsString gitHubRepoUrl (tagFromVersionNumber newVersion.AsString)
    else
        let versionTuple version = (version.Major, version.Minor, version.Patch)
        // Changelog entries come already sorted, most-recent first, by the Changelog module
        let prevEntry =
            changelog.Entries
            |> List.skipWhile (fun entry ->
                entry.SemVer.PreRelease.IsSome
                || versionTuple entry.SemVer = versionTuple newVersion
            )
            |> List.tryHead

        let linkTarget =
            match prevEntry with
            | Some entry ->
                sprintf
                    "%s/compare/%s...%s"
                    gitHubRepoUrl
                    (tagFromVersionNumber entry.SemVer.AsString)
                    (tagFromVersionNumber newVersion.AsString)
            | None -> sprintf "%s/releases/tag/%s" gitHubRepoUrl (tagFromVersionNumber newVersion.AsString)

        sprintf "[%s]: %s" newVersion.AsString linkTarget

let mkReleaseNotes changelog (latestEntry : Changelog.ChangelogEntry) gitHubRepoUrl =
    let linkReference = mkLinkReference latestEntry.SemVer changelog gitHubRepoUrl

    if String.isNullOrEmpty linkReference then
        latestEntry.ToString ()
    else
        // Add link reference target to description before building release notes, since in main changelog file it's at the bottom of the file
        let description =
            match latestEntry.Description with
            | None -> linkReference
            | Some desc when desc.Contains (linkReference) -> desc
            | Some desc -> sprintf "%s\n\n%s" (desc.Trim ()) linkReference

        { latestEntry with Description = Some description }.ToString ()

let getVersionNumber envVarName ctx =
    let args = ctx.Context.Arguments

    let verArg =
        args
        |> List.tryHead
        |> Option.defaultWith (fun () -> Environment.environVarOrDefault envVarName "")

    if SemVer.isValid verArg then
        verArg
    elif verArg.StartsWith ("v") && SemVer.isValid verArg.[1..] then
        let target = ctx.Context.FinalTarget

        Trace.traceImportantfn
            "Please specify a version number without leading 'v' next time, e.g. \"./build.sh %s %s\" rather than \"./build.sh %s %s\""
            target
            verArg.[1..]
            target
            verArg

        verArg.[1..]
    elif String.isNullOrEmpty verArg then
        let target = ctx.Context.FinalTarget

        Trace.traceErrorfn
            "Please specify a version number, either at the command line (\"./build.sh %s 1.0.0\") or in the %s environment variable"
            target
            envVarName

        failwith "No version number found"
    else
        Trace.traceErrorfn "Please specify a valid version number: %A could not be recognized as a version number" verArg

        failwith "Invalid version number"

let mutable changelogBackupFilename = ""

let updateChangelog changelogPath (changelog : Fake.Core.Changelog.Changelog) gitHubRepoUrl ctx =

    let verStr = ctx |> getVersionNumber "RELEASE_VERSION"

    let description, unreleasedChanges =
        match changelog.Unreleased with
        | None -> None, []
        | Some u -> u.Description, u.Changes

    let newVersion = SemVer.parse verStr

    changelog.Entries
    |> List.tryFind (fun entry -> entry.SemVer = newVersion)
    |> Option.iter (fun entry ->
        Trace.traceErrorfn
            "Version %s already exists in %s, released on %s"
            verStr
            changelogPath
            (if entry.Date.IsSome then
                 entry.Date.Value.ToString ("yyyy-MM-dd")
             else
                 "(no date specified)")

        failwith "Can't release with a duplicate version number"
    )

    changelog.Entries
    |> List.tryFind (fun entry -> entry.SemVer > newVersion)
    |> Option.iter (fun entry ->
        Trace.traceErrorfn
            "You're trying to release version %s, but a later version %s already exists, released on %s"
            verStr
            entry.SemVer.AsString
            (if entry.Date.IsSome then
                 entry.Date.Value.ToString ("yyyy-MM-dd")
             else
                 "(no date specified)")

        failwith "Can't release with a version number older than an existing release"
    )

    let versionTuple version = (version.Major, version.Minor, version.Patch)

    let prereleaseEntries =
        changelog.Entries
        |> List.filter (fun entry ->
            entry.SemVer.PreRelease.IsSome
            && versionTuple entry.SemVer = versionTuple newVersion
        )

    let prereleaseChanges =
        prereleaseEntries
        |> List.collect (fun entry -> entry.Changes |> List.filter (not << isEmptyChange))
        |> List.distinct

    let assemblyVersion, nugetVersion = Changelog.parseVersions newVersion.AsString

    let newEntry =
        Changelog.ChangelogEntry.New (
            assemblyVersion.Value,
            nugetVersion.Value,
            Some System.DateTime.Today,
            description,
            unreleasedChanges @ prereleaseChanges,
            false
        )

    let newChangelog =
        Changelog.Changelog.New (changelog.Header, changelog.Description, None, newEntry :: changelog.Entries)

    // Save changelog to temporary file before making any edits
    changelogBackupFilename <- System.IO.Path.GetTempFileName ()

    changelogPath |> Shell.copyFile changelogBackupFilename

    Target.activateFinal "DeleteChangelogBackupFile"

    newChangelog |> Changelog.save changelogPath

    // Now update the link references at the end of the file
    let linkReferenceForLatestEntry = mkLinkReference newVersion changelog gitHubRepoUrl

    let linkReferenceForUnreleased =
        sprintf "[Unreleased]: %s/compare/%s...%s" gitHubRepoUrl (tagFromVersionNumber newVersion.AsString) "HEAD"

    let tailLines = File.read changelogPath |> List.ofSeq |> List.rev

    let isRef (line : string) =
        System.Text.RegularExpressions.Regex.IsMatch (line, @"^\[.+?\]:\s?[a-z]+://.*$")

    let linkReferenceTargets =
        tailLines
        |> Seq.skipWhile String.isNullOrWhiteSpace
        |> Seq.takeWhile isRef
        |> Seq.rev // Now most recent entry is at the head of the list
        |> Seq.toList

    let newLinkReferenceTargets =
        match linkReferenceTargets with
        | [] -> [ linkReferenceForUnreleased; linkReferenceForLatestEntry ]
        | first :: rest when first |> String.startsWith "[Unreleased]:" ->
            linkReferenceForUnreleased
            :: linkReferenceForLatestEntry
            :: rest
        | first :: rest ->
            linkReferenceForUnreleased
            :: linkReferenceForLatestEntry
            :: first
            :: rest

    let blankLineCount =
        tailLines
        |> Seq.takeWhile String.isNullOrWhiteSpace
        |> Seq.length

    let linkRefCount = linkReferenceTargets |> List.length

    let skipCount = blankLineCount + linkRefCount

    let updatedLines =
        List.rev (tailLines |> List.skip skipCount)
        @ newLinkReferenceTargets

    System.IO.File.WriteAllLines (changelogPath, updatedLines)

    // If build fails after this point but before we commit changes, undo our modifications
    Target.activateBuildFailure "RevertChangelog"

    newEntry
