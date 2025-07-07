open System
open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api
open Fake.BuildServer
open Argu

let environVarAsBoolOrDefault varName defaultValue =
    let truthyConsts = [ "1"; "Y"; "YES"; "T"; "TRUE" ]
    Environment.environVar varName
    |> ValueOption.ofObj
    |> ValueOption.map (fun envvar ->
        truthyConsts
        |> List.exists (fun ``const`` -> String.Equals (``const``, envvar, StringComparison.InvariantCultureIgnoreCase))
    )
    |> ValueOption.defaultValue defaultValue

//-----------------------------------------------------------------------------
// Metadata and Configuration
//-----------------------------------------------------------------------------

let rootDirectory = __SOURCE_DIRECTORY__ </> ".."

let productName = "FSharpLint"

let sln = rootDirectory </> "FSharpLint.slnf"

let srcCodeGlob =
    !!(rootDirectory </> "src/**/*.fs")
    ++ (rootDirectory </> "src/**/*.fsx")
    -- (rootDirectory </> "src/**/obj/**/*.fs")

let testsCodeGlob =
    !!(rootDirectory </> "tests/**/*.fs")
    ++ (rootDirectory </> "tests/**/*.fsx")
    -- (rootDirectory </> "tests/**/obj/**/*.fs")

let srcGlob = rootDirectory </> "src/**/*.??proj"

let testsGlob = rootDirectory </> "tests/**/*.??proj"

let srcAndTest = !!srcGlob ++ testsGlob

let distDir = rootDirectory </> "dist"

let distGlob = distDir </> "*.nupkg"

let docsDir = rootDirectory </> "docs"

let docsSrcDir = rootDirectory </> "docsSrc"

let temp = rootDirectory </> "temp"

let watchDocsDir = temp </> "watch-docs"

let gitOwner = "fsprojects"
let gitRepoName = "FSharpLint"

let gitHubRepoUrl = $"https://github.com/%s{gitOwner}/%s{gitRepoName}"

let documentationRootUrl = $"https://%s{gitOwner}.github.io/%s{gitRepoName}"

let releaseBranch = "main"
let readme = "README.md"
let changelogFile = "CHANGELOG.md"

let READMElink = Uri (Uri (gitHubRepoUrl), $"blob/{releaseBranch}/{readme}")
let CHANGELOGlink = Uri (Uri (gitHubRepoUrl), $"blob/{releaseBranch}/{changelogFile}")

let changelogPath = rootDirectory </> changelogFile

let changelog = Fake.Core.Changelog.load changelogPath

let mutable latestEntry =
    if Seq.isEmpty changelog.Entries then
        Changelog.ChangelogEntry.New ("0.0.1", "0.0.1-alpha.1", Some DateTime.Today, None, [], false)
    else
        changelog.LatestEntry

let mutable changelogBackupFilename = ""

let publishUrl = "https://www.nuget.org"

let githubToken = Environment.environVarOrNone "GITHUB_TOKEN"

let nugetToken = Environment.environVarOrNone "NUGET_KEY"

//-----------------------------------------------------------------------------
// Helpers
//-----------------------------------------------------------------------------

let isRelease (targets : Target list) =
    targets
    |> Seq.map (fun t -> t.Name)
    |> Seq.exists ((=) "PublishToNuGet")

let invokeAsync f = async { f () }

let configuration (targets : Target list) =
    let defaultVal = if isRelease targets then "Release" else "Debug"

    match Environment.environVarOrDefault "CONFIGURATION" defaultVal with
    | "Debug" -> DotNet.BuildConfiguration.Debug
    | "Release" -> DotNet.BuildConfiguration.Release
    | config -> DotNet.BuildConfiguration.Custom config

let failOnBadExitAndPrint (p : ProcessResult) =
    if p.ExitCode <> 0 then
        p.Errors |> Seq.iter Trace.traceError

        failwithf "failed with exitcode %d" p.ExitCode

let isPublishToGitHub ctx = ctx.Context.FinalTarget = "PublishToGitHub"

let isCI = lazy environVarAsBoolOrDefault "CI" false

// CI Servers can have bizarre failures that have nothing to do with your code
let rec retryIfInCI times fn =
    match isCI.Value with
    | true ->
        if times > 1 then
            try
                fn ()
            with _ ->
                retryIfInCI (times - 1) fn
        else
            fn ()
    | _ -> fn ()

let failOnWrongBranch () =
    if Git.Information.getBranchName "" <> releaseBranch then
        failwithf "Not on %s.  If you want to release please switch to this branch." releaseBranch


module dotnet =
    let watch cmdParam program args = DotNet.exec cmdParam ($"watch %s{program}") args

    let run cmdParam args = DotNet.exec cmdParam "run" args

    let tool optionConfig (command : string) args =
        DotNet.exec optionConfig command args
        |> failOnBadExitAndPrint

    let sourcelink optionConfig args = tool optionConfig "sourcelink" args

    let fcswatch optionConfig args = tool optionConfig "fcswatch" args

    let fsharpAnalyzer optionConfig args = tool optionConfig "fsharp-analyzers" args

    let fantomas args = DotNet.exec id "fantomas" args

module FSharpAnalyzers =
    type Arguments =
        | Project of string
        | Analyzers_Path of string
        | Fail_On_Warnings of string list
        | Ignore_Files of string list
        | Verbose

        interface IArgParserTemplate with
            member s.Usage = ""


module DocsTool =
    /// <summary>
    /// Clean Fornax cache and generated files
    /// </summary>
    let cleanDocsCache () = Fornax.cleanCache docsDir

    /// <summary>
    /// Build documentation using Fornax
    /// </summary>
    let build (configuration) =
        let result = Fornax.build (fun p -> { p with WorkingDirectory = Some docsDir })
        result |> ignore

    /// <summary>
    /// Watch documentation using Fornax with hot reload
    /// </summary>
    let watch (configuration) =
        let result = Fornax.watch (fun p -> { p with WorkingDirectory = Some docsDir })
        result |> ignore

let allReleaseChecks () = failOnWrongBranch ()
//Changelog.failOnEmptyChangelog latestEntry

let failOnLocalBuild () =
    if not isCI.Value then
        failwith "Not on CI. If you want to publish, please use CI."

let failOnCIBuild () =
    if isCI.Value then
        failwith "On CI. If you want to run this target, please use a local build."

let allPublishChecks () = failOnLocalBuild ()
//Changelog.failOnEmptyChangelog latestEntry

//-----------------------------------------------------------------------------
// Target Implementations
//-----------------------------------------------------------------------------

/// So we don't require always being on the latest MSBuild.StructuredLogger
let disableBinLog (p : MSBuild.CliArguments) = { p with DisableInternalBinLog = true }

let clean _ =
    [ "bin"; "temp"; distDir ]
    |> Shell.cleanDirs

    !!srcGlob ++ testsGlob
    |> Seq.collect (fun p ->
        [ "bin"; "obj" ]
        |> Seq.map (fun sp -> IO.Path.GetDirectoryName p </> sp)
    )
    |> Shell.cleanDirs

let dotnetRestore _ =
    [ sln ]
    |> Seq.map (fun dir ->
        fun () ->
            let args = [] |> String.concat " "

            DotNet.restore
                (fun c -> {
                    c with
                        MSBuildParams = disableBinLog c.MSBuildParams
                        Common = c.Common |> DotNet.Options.withCustomParams (Some (args))
                })
                dir
    )
    |> Seq.iter (retryIfInCI 10)

let dotnetToolRestore _ =
    let result =
        fun () -> DotNet.exec id "tool" "restore"
        |> (retryIfInCI 10)

    if not result.OK then
        failwithf "Failed to restore .NET tools: %A" result.Errors

let updateChangelog ctx =
    latestEntry <-
        if not <| isPublishToGitHub ctx then
            Changelog.updateChangelog changelogPath changelog gitHubRepoUrl ctx
        elif Seq.isEmpty changelog.Entries then
            latestEntry
        else
            let latest = changelog.LatestEntry
            let semVer = {
                latest.SemVer with
                    Original = None
                    Patch = latest.SemVer.Patch + 1u
                    PreRelease = PreRelease.TryParse "ci"
            }
            {
                latest with
                    SemVer = semVer
                    NuGetVersion = semVer.AsString
                    AssemblyVersion = semVer.AsString
            }

let revertChangelog _ =
    if String.isNotNullOrEmpty Changelog.changelogBackupFilename then
        Changelog.changelogBackupFilename
        |> Shell.copyFile changelogPath

let deleteChangelogBackupFile _ =
    if String.isNotNullOrEmpty Changelog.changelogBackupFilename then
        Shell.rm Changelog.changelogBackupFilename

let getPackageVersionProperty publishToGitHub =
    if publishToGitHub then
        let runId = Environment.environVar "GITHUB_RUN_ID"
        $"/p:PackageVersion=%s{latestEntry.NuGetVersion}-%s{runId}"
    else
        $"/p:PackageVersion=%s{latestEntry.NuGetVersion}"

let dotnetBuild ctx =

    let publishToGitHub = isPublishToGitHub ctx

    let args = [ getPackageVersionProperty publishToGitHub; "--no-restore" ]

    DotNet.build
        (fun c -> {
            c with
                Configuration = configuration (ctx.Context.AllExecutingTargets)
                Common = c.Common |> DotNet.Options.withAdditionalArgs args
                MSBuildParams = {
                    (disableBinLog c.MSBuildParams) with
                        Properties = [
                            if publishToGitHub then
                                ("DebugType", "embedded")
                                ("EmbedAllSources", "true")
                        ]
                }
        })
        sln

let fsharpAnalyzers _ =
    let argParser = ArgumentParser.Create<FSharpAnalyzers.Arguments> (programName = "fsharp-analyzers")

    !!srcGlob
    |> Seq.iter (fun proj ->
        let args =
            [
                FSharpAnalyzers.Analyzers_Path (rootDirectory </> "packages/analyzers")
                FSharpAnalyzers.Arguments.Project proj
                FSharpAnalyzers.Arguments.Fail_On_Warnings [ "BDH0002" ]
                FSharpAnalyzers.Arguments.Ignore_Files [ "*AssemblyInfo.fs" ]
                FSharpAnalyzers.Verbose
            ]
            |> argParser.PrintCommandLineArgumentsFlat

        dotnet.fsharpAnalyzer id args
    )

let dotnetTest ctx =
    let args = [ "--no-build" ]

    // Filter performance tests like in build.fsx
    let filterPerformanceTests (p : DotNet.TestOptions) = {
        p with
            Filter = Some "\"TestCategory!=Performance\""
            Configuration = configuration (ctx.Context.AllExecutingTargets)
    }

    // Run the same test projects as in build.fsx
    DotNet.test
        (filterPerformanceTests
         >> fun opts -> {
             opts with
                 MSBuildParams = disableBinLog opts.MSBuildParams
                 Common =
                     opts.Common
                     |> DotNet.Options.withAdditionalArgs args
         })
        (rootDirectory </> "tests/FSharpLint.Core.Tests")

    DotNet.test
        (filterPerformanceTests
         >> fun opts -> {
             opts with
                 MSBuildParams = disableBinLog opts.MSBuildParams
                 Common =
                     opts.Common
                     |> DotNet.Options.withAdditionalArgs args
         })
        (rootDirectory </> "tests/FSharpLint.Console.Tests")

    // Restore the functional test project like in build.fsx
    DotNet.restore id (rootDirectory </> "tests/FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProject.sln")

    DotNet.test
        (filterPerformanceTests
         >> fun opts -> {
             opts with
                 MSBuildParams = disableBinLog opts.MSBuildParams
                 Common =
                     opts.Common
                     |> DotNet.Options.withAdditionalArgs args
         })
        (rootDirectory </> "tests/FSharpLint.FunctionalTest")

let watchTests _ =
    !!testsGlob
    |> Seq.map (fun proj ->
        fun () ->
            dotnet.watch
                (fun opt ->
                    opt
                    |> DotNet.Options.withWorkingDirectory (IO.Path.GetDirectoryName proj)
                )
                "test"
                ""
            |> ignore
    )
    |> Seq.iter (invokeAsync >> Async.Catch >> Async.Ignore >> Async.Start)

    printfn "Press Ctrl+C (or Ctrl+Break) to stop..."

    let cancelEvent =
        Console.CancelKeyPress
        |> Async.AwaitEvent
        |> Async.RunSynchronously

    cancelEvent.Cancel <- true

let generateAssemblyInfo _ =

    let (|Fsproj|Csproj|Vbproj|) (projFileName : string) =
        match projFileName with
        | f when f.EndsWith ("fsproj") -> Fsproj
        | f when f.EndsWith ("csproj") -> Csproj
        | f when f.EndsWith ("vbproj") -> Vbproj
        | _ -> failwith $"Project file %s{projFileName} not supported. Unknown project type."

    let releaseChannel =
        match latestEntry.SemVer.PreRelease with
        | Some pr -> pr.Name
        | _ -> "release"

    let getAssemblyInfoAttributes projectName = [
        AssemblyInfo.Title (projectName)
        AssemblyInfo.Product productName
        AssemblyInfo.Version latestEntry.AssemblyVersion
        AssemblyInfo.Metadata ("ReleaseDate", latestEntry.Date.Value.ToString ("o"))
        AssemblyInfo.FileVersion latestEntry.AssemblyVersion
        AssemblyInfo.InformationalVersion latestEntry.AssemblyVersion
        AssemblyInfo.Metadata ("ReleaseChannel", releaseChannel)
        AssemblyInfo.Metadata ("GitHash", Git.Information.getCurrentSHA1 (null))
    ]

    let getProjectDetails (projectPath : string) =
        let projectName = IO.Path.GetFileNameWithoutExtension (projectPath)

        (projectPath, projectName, IO.Path.GetDirectoryName (projectPath), (getAssemblyInfoAttributes projectName))

    !!srcGlob
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
        match projFileName with
        | Fsproj -> AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
        | Csproj -> AssemblyInfoFile.createCSharp ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
        | Vbproj -> AssemblyInfoFile.createVisualBasic ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
    )

let dotnetPack ctx =
    // Get release notes with properly-linked version number
    let releaseNotes = Changelog.mkReleaseNotes changelog latestEntry gitHubRepoUrl

    let args = [ getPackageVersionProperty (isPublishToGitHub ctx); $"/p:PackageReleaseNotes=\"{releaseNotes}\"" ]

    DotNet.pack
        (fun c -> {
            c with
                MSBuildParams = disableBinLog c.MSBuildParams
                Configuration = configuration (ctx.Context.AllExecutingTargets)
                OutputPath = Some distDir
                Common = c.Common |> DotNet.Options.withAdditionalArgs args
        })
        sln

let sourceLinkTest _ =
    !!distGlob
    |> Seq.iter (fun nupkg -> dotnet.sourcelink id $"test %s{nupkg}")

type PushSource =
    | NuGet
    | GitHub

let publishTo (source : PushSource) _ =
    allPublishChecks ()

    distGlob
    |> DotNet.nugetPush (fun o -> {
        o with
            Common = {
                o.Common with
                    WorkingDirectory = "dist"
                    CustomParams = Some "--skip-duplicate"
            }
            PushParams = {
                o.PushParams with
                    NoSymbols = source.IsGitHub
                    Source =
                        match source with
                        | NuGet -> Some "nuget.org"
                        | GitHub -> Some "github.com"
                    ApiKey =
                        match source with
                        | NuGet -> nugetToken
                        | GitHub -> githubToken
            }
    })

let gitRelease _ =
    allReleaseChecks ()

    let releaseNotesGitCommitFormat = latestEntry.ToString ()

    Git.Staging.stageFile "" (rootDirectory </> "CHANGELOG.md")
    |> ignore

    !!(rootDirectory </> "src/**/AssemblyInfo.fs")
    ++ (rootDirectory </> "tests/**/AssemblyInfo.fs")
    |> Seq.iter (Git.Staging.stageFile "" >> ignore)

    let msg = $"Bump version to `%s{latestEntry.NuGetVersion}`\n\n%s{releaseNotesGitCommitFormat}"

    Git.Commit.exec "" msg

    Target.deactivateBuildFailure "RevertChangelog"

    Git.Branches.push ""

    let tag = Changelog.tagFromVersionNumber latestEntry.NuGetVersion

    Git.Branches.tag "" tag
    Git.Branches.pushTag "" "origin" tag

let githubRelease _ =
    allPublishChecks ()

    let token =
        match githubToken with
        | Some s -> s
        | _ -> failwith "please set the `GITHUB_TOKEN` environment variable to a github personal access token with repo access."

    let files = !!distGlob
    // Get release notes with properly-linked version number
    let releaseNotes = Changelog.mkReleaseNotes changelog latestEntry gitHubRepoUrl

    GitHub.createClientWithToken token
    |> GitHub.draftNewRelease
        gitOwner
        gitRepoName
        (Changelog.tagFromVersionNumber latestEntry.NuGetVersion)
        (latestEntry.SemVer.PreRelease <> None)
        (releaseNotes |> Seq.singleton)
    |> GitHub.uploadFiles files
    |> GitHub.publishDraft
    |> Async.RunSynchronously

let formatCode _ =
    let result = dotnet.fantomas $"{rootDirectory}"

    if not result.OK then
        printfn "Errors while formatting all files: %A" result.Messages

let checkFormatCode ctx =
    let result = dotnet.fantomas $"{rootDirectory} --check"

    if result.ExitCode = 0 then
        Trace.log "No files need formatting"
    elif result.ExitCode = 99 then
        failwith "Some files need formatting, check output for more info"
    else
        Trace.logf "Errors while formatting: %A" result.Errors


let cleanDocsCache _ = DocsTool.cleanDocsCache ()

let buildDocs ctx =
    let configuration = configuration (ctx.Context.AllExecutingTargets)

    // Build only FSharpLint.Core project for documentation
    DotNet.build
        (fun c -> {
            c with
                Configuration = DotNet.BuildConfiguration.fromString (string configuration)
                MSBuildParams = disableBinLog c.MSBuildParams
        })
        (rootDirectory </> "src/FSharpLint.Core")

    DocsTool.build (string configuration)

let watchDocs ctx =
    let configuration = configuration (ctx.Context.AllExecutingTargets)
    DocsTool.watch (string configuration)


let initTargets (ctx : Context.FakeExecutionContext) =
    BuildServer.install [ GitHubActions.Installer ]

    let isPublishToGitHub =
        ctx.Arguments
        |> Seq.pairwise
        |> Seq.exists (fun (arg, value) ->
            (String.Equals (arg, "-t", StringComparison.OrdinalIgnoreCase)
             || String.Equals (arg, "--target", StringComparison.OrdinalIgnoreCase))
            && String.Equals (value, "PublishToGitHub", StringComparison.OrdinalIgnoreCase)
        )

    /// Defines a dependency - y is dependent on x. Finishes the chain.
    let (==>!) x y = x ==> y |> ignore

    /// Defines a soft dependency. x must run before y, if it is present, but y does not require x to be run. Finishes the chain.
    let (?=>!) x y = x ?=> y |> ignore
    //-----------------------------------------------------------------------------
    // Hide Secrets in Logger
    //-----------------------------------------------------------------------------
    Option.iter (TraceSecrets.register "<GITHUB_TOKEN>") githubToken
    Option.iter (TraceSecrets.register "<NUGET_KEY>") nugetToken
    //-----------------------------------------------------------------------------
    // Target Declaration
    //-----------------------------------------------------------------------------

    Target.create "Clean" clean
    Target.create "DotnetRestore" dotnetRestore
    Target.create "DotnetToolRestore" dotnetToolRestore
    Target.create "UpdateChangelog" updateChangelog
    Target.createBuildFailure "RevertChangelog" revertChangelog // Do NOT put this in the dependency chain
    Target.createFinal "DeleteChangelogBackupFile" deleteChangelogBackupFile // Do NOT put this in the dependency chain
    Target.create "DotnetBuild" dotnetBuild
    Target.create "FSharpAnalyzers" fsharpAnalyzers
    Target.create "DotnetTest" dotnetTest
    Target.create "WatchTests" watchTests
    Target.create "GenerateAssemblyInfo" generateAssemblyInfo
    Target.create "DotnetPack" dotnetPack
    Target.create "SourceLinkTest" sourceLinkTest
    Target.create "PublishToNuGet" (publishTo NuGet)
    Target.create "PublishToGitHub" (publishTo GitHub)
    Target.create "GitRelease" gitRelease
    Target.create "GitHubRelease" githubRelease
    Target.create "FormatCode" formatCode
    Target.create "CheckFormatCode" checkFormatCode
    Target.create "Release" ignore // For local
    Target.create "Publish" ignore //For CI
    Target.create "CleanDocsCache" cleanDocsCache
    Target.create "BuildDocs" buildDocs
    Target.create "WatchDocs" watchDocs

    //-----------------------------------------------------------------------------
    // Target Dependencies
    //-----------------------------------------------------------------------------

    // Only call Clean if DotnetPack was in the call chain
    // Ensure Clean is called before DotnetRestore
    "Clean" ?=>! "DotnetRestore"

    "Clean" ==>! "DotnetPack"

    // Only call GenerateAssemblyInfo if GitRelease was in the call chain
    // Ensure GenerateAssemblyInfo is called after DotnetRestore and before DotnetBuild
    "DotnetRestore" ?=>! "GenerateAssemblyInfo"

    "GenerateAssemblyInfo" ?=>! "DotnetBuild"

    // Ensure UpdateChangelog is called after DotnetRestore
    "DotnetRestore" ?=>! "UpdateChangelog"

    "UpdateChangelog" ?=>! "GenerateAssemblyInfo"

    "CleanDocsCache" ==>! "BuildDocs"

    // BuildDocs doesn't need DotnetBuild as it builds FSharpLint.Core itself
    // "DotnetBuild" ?=>! "BuildDocs"
    // "DotnetBuild" ==>! "BuildDocs"

    "DotnetBuild" ==>! "WatchDocs"

    "UpdateChangelog"
    ==> "GenerateAssemblyInfo"
    ==> "GitRelease"
    ==>! "Release"


    "DotnetRestore" =?> ("CheckFormatCode", isCI.Value)
    ==> "DotnetBuild"
    ==> "DotnetTest"
    ==> "DotnetPack"
    ==> "PublishToNuGet"
    ==> "GitHubRelease"
    ==>! "Publish"

    "DotnetRestore"
    =?> ("CheckFormatCode", isCI.Value)
    =?> ("GenerateAssemblyInfo", isPublishToGitHub)
    ==> "DotnetBuild"
    ==> "DotnetTest"
    ==> "DotnetPack"
    ==>! "PublishToGitHub"

    "DotnetRestore" ==>! "WatchTests"

    "DotnetToolRestore" ?=>! "DotnetRestore"
    "DotnetToolRestore" ==>! "BuildDocs"
    "DotnetToolRestore" ?=>! "CheckFormatCode"
    "DotnetToolRestore" ?=>! "FormatCode"

//-----------------------------------------------------------------------------
// Target Start
//-----------------------------------------------------------------------------
[<EntryPoint>]
let main argv =

    let ctx =
        argv
        |> Array.toList
        |> Context.FakeExecutionContext.Create false "build.fsx"

    Context.setExecutionContext (Context.RuntimeContext.Fake ctx)
    initTargets ctx
    Target.runOrDefaultWithArguments "DotnetPack"

    0 // return an integer exit code
