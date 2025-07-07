// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "nuget: MSBuild.StructuredLogger"
#r "nuget: Fake.Core"
#r "nuget: Fake.Core.Target"
#r "nuget: Fake.Core.Process"
#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.Core.ReleaseNotes"
#r "nuget: Fake.DotNet.AssemblyInfoFile"
#r "nuget: Fake.Tools.Git"
#r "nuget: Fake.Core.Environment"
#r "nuget: Fake.Core.UserInput"
#r "nuget: Fake.IO.FileSystem"
#r "nuget: Fake.DotNet.MsBuild"
#r "nuget: Fake.Api.GitHub"

#if FAKE
#load ".fake/build.fsx/intellisense.fsx"
#else
// Boilerplate
System.Environment.GetCommandLineArgs()
|> Array.skip 2 // skip fsi.exe; build.fsx
|> Array.toList
|> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext

#endif

open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api

open System
open System.IO

Target.initEnvironment()

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "FSharpLint"
let solutionFileName = "FSharpLint.slnx"

let authors = "Matthew Mcveigh"

let gitOwner = "fsprojects"
let gitName = "FSharpLint"
let gitHome = $"https://github.com/{gitOwner}"
let gitUrl = $"{gitHome}/{gitName}"

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
let isNullOrWhiteSpace = System.String.IsNullOrWhiteSpace

let exec cmd args dir =
    let proc =
        CreateProcess.fromRawCommandLine cmd args
        |> CreateProcess.ensureExitCodeWithMessage $"Error while running '%s{cmd}' with args: %s{args}"
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
let rootDir = __SOURCE_DIRECTORY__ |> DirectoryInfo

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
        ("PackageReleaseNotes", $"%s{gitUrl}/blob/v%s{nugetVersion}/CHANGELOG.md")::baseProps
    else
        baseProps

// --------------------------------------------------------------------------------------
// Build Targets
// --------------------------------------------------------------------------------------

Target.create "Clean" (fun _ ->
    Shell.cleanDirs [buildDir; nugetDir]
)

Target.create "Build" (fun _ ->
    DotNet.build id solutionFileName
)

let filterPerformanceTests (p:DotNet.TestOptions) = { p with Filter = Some "\"TestCategory!=Performance\""; Configuration = DotNet.Release }

Target.create "Test" (fun _ ->
  DotNet.test filterPerformanceTests "tests/FSharpLint.Core.Tests"
  DotNet.test filterPerformanceTests "tests/FSharpLint.Console.Tests"
  DotNet.restore id "tests/FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProject.sln"
  DotNet.test filterPerformanceTests "tests/FSharpLint.FunctionalTest"
)

Target.create "Docs" (fun _ ->
    exec "dotnet"  @"fornax build" "docs"
)

// --------------------------------------------------------------------------------------
// Release Targets
// --------------------------------------------------------------------------------------

Target.create "BuildRelease" (fun _ ->
    let properties = ("Version", nugetVersion) |> List.singleton |> PackageReleaseNotes

    DotNet.build (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            OutputPath = Some buildDir
            MSBuildParams = { p.MSBuildParams with Properties = properties }
        }
    ) solutionFileName
)


Target.create "Pack" (fun _ ->
    let properties = PackageReleaseNotes ([
        ("Version", nugetVersion);
        ("Authors", authors)
        ("PackageProjectUrl", gitUrl)
        ("RepositoryType", "git")
        ("RepositoryUrl", gitUrl)
        ("PackageLicenseExpression", "MIT")
    ])

    DotNet.pack (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            OutputPath = Some nugetDir
            MSBuildParams = { p.MSBuildParams with Properties = properties }
        }
    ) solutionFileName
)

Target.create "Push" (fun _ ->
    let push key =
        let distGlob = nugetDir </> "*.nupkg"
        distGlob
        |> DotNet.nugetPush (fun o -> {
            o with
                Common = {
                    o.Common with
                        CustomParams = Some "--skip-duplicate"
                }
                PushParams = {
                    o.PushParams with
                        Source = Some "https://api.nuget.org/v3/index.json"
                        ApiKey = Some key
                }
        })

    let key = getBuildParam "nuget-key"
    match getBuildParam "GITHUB_EVENT_NAME" with
    | None ->
        match key with
        | None ->
            let key = UserInput.getUserPassword "NuGet Key: "
            push key
        | Some key ->
            push key

    | Some "push" ->
        match key with
        | None ->
            Console.WriteLine "No nuget-key env var found, skipping..."
        | Some key ->
            if isTag then
                push key
            elif getBuildParam "GITHUB_REF_NAME" <> Some "master" then
                Console.WriteLine "Not a push to master branch, skipping..."
            else
                match getBuildParam "GITHUB_SHA" with
                | None ->
                    failwith "GITHUB_SHA should have been populated"
                | Some commitHash ->
                    let gitArgs = $"describe --exact-match --tags %s{commitHash}"
                    let proc =
                        CreateProcess.fromRawCommandLine "git" gitArgs
                        |> Proc.run
                    if proc.ExitCode <> 0 then
                        // commit is not a tag, so go ahead pushing a prerelease
                        push key
                    else
                        Console.WriteLine "Commit mapped to a tag, skipping pushing prerelease..."
    | _ ->
        Console.WriteLine "Github event name not 'push', skipping..."

)


Target.create "SelfCheck" (fun _ ->
    let srcDir = Path.Combine(rootDir.FullName, "src") |> DirectoryInfo

    let consoleProj = Path.Combine(srcDir.FullName, "FSharpLint.Console", "FSharpLint.Console.fsproj") |> FileInfo
    let sol = Path.Combine(rootDir.FullName, solutionFileName) |> FileInfo
    exec "dotnet" $"run lint %s{sol.FullName}" consoleProj.Directory.FullName
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
Target.create "Default" DoNothing
Target.create "Release" DoNothing

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "Default"

"Clean"
 ==> "BuildRelease"
 ==> "Docs"

"Default"
  ==> "Pack"
  ==> "Push"
  ==> "Release"

Target.runOrDefaultWithArguments "Default"
