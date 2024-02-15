#r "nuget: Fake.Core.Process"
#r "nuget: Fake.Core.ReleaseNotes"

open System
open System.IO

open Fake.Core


let project = "FSharpLint"

let authors = "Matthew Mcveigh"

let gitOwner = "fsprojects"
let gitName = "FSharpLint"
let gitHome = "https://github.com/" + gitOwner
let gitUrl = gitHome + "/" + gitName

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
let isNullOrWhiteSpace = System.String.IsNullOrWhiteSpace

let exec cmd args dir =
    let proc =
        CreateProcess.fromRawCommandLine cmd args
        |> CreateProcess.ensureExitCodeWithMessage (sprintf "Error while running '%s' with args: %s" cmd args)
    (if isNullOrWhiteSpace dir then proc
    else proc |> CreateProcess.withWorkingDirectory dir)
    |> Proc.run
    |> ignore

let getBuildParam var =
    let value = Environment.environVar var
    if String.IsNullOrWhiteSpace value then
        None
    else
        Some value
let DoNothing = ignore

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

let buildDir  = "./build/"
let nugetDir  = "./out/"
let rootDir = (__SOURCE_DIRECTORY__ |> DirectoryInfo).Parent

System.Environment.CurrentDirectory <- rootDir.FullName
let changelogFilename = "CHANGELOG.md"
let changelog = Changelog.load changelogFilename

let githubRef = Environment.GetEnvironmentVariable "GITHUB_REF"
let tagPrefix = "refs/tags/"
let isTag =
    if isNull githubRef then
        false
    else
        githubRef.StartsWith tagPrefix

let nugetVersion =
    match (changelog.Unreleased, isTag) with
    | (Some _unreleased, true) -> failwith "Shouldn't publish a git tag for changes outside a real release"
    | (None, true) ->
        changelog.LatestEntry.NuGetVersion
    | (_, false) ->
        let current = changelog.LatestEntry.NuGetVersion |> SemVer.parse
        let bumped = { current with
                            Patch = current.Patch + 1u
                            Original = None
                            PreRelease = None }
        let bumpedBaseVersion = string bumped

        let nugetPreRelease = Path.Combine(rootDir.FullName, "nugetPreRelease.fsx")
        let procResult =
            CreateProcess.fromRawCommand
                "dotnet"
                [
                    "fsi"
                    nugetPreRelease
                    bumpedBaseVersion
                ]
            |> CreateProcess.redirectOutput
            |> CreateProcess.ensureExitCode
            |> Proc.run
        procResult.Result.Output.Trim()

let PackageReleaseNotes baseProps =
    if isTag then
        ("PackageReleaseNotes", sprintf "%s/blob/v%s/CHANGELOG.md" gitUrl nugetVersion)::baseProps
    else
        baseProps
