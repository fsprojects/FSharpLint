// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "FSharpLint"

let authors = "Matthew Mcveigh"

let gitOwner = "fsprojects"
let gitName = "FSharpLint"
let gitHome = "https://github.com/" + gitOwner
let gitUrl = gitHome + "/" + gitName

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

let buildDir  = "./build/"
let nugetDir  = "./out/"


System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let changelogFilename = "CHANGELOG.md"
let changelog = Changelog.load changelogFilename
let latestEntry = changelog.LatestEntry

let nugetVersion = latestEntry.NuGetVersion
let packageReleaseNotes = sprintf "%s/blob/v%s/CHANGELOG.md" gitUrl latestEntry.NuGetVersion
let releaseNotes =
    latestEntry.Changes
    |> List.map (fun c -> " * " + c.ToString())
    |> String.concat "\n"

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

let getBuildParam = Environment.environVar
let DoNothing = ignore
// --------------------------------------------------------------------------------------
// Build Targets
// --------------------------------------------------------------------------------------

Target.create "Clean" (fun _ ->
    Shell.cleanDirs [buildDir; nugetDir]
)

Target.create "Build" (fun _ ->
    DotNet.build id "FSharpLint.sln"
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
    DotNet.build (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            OutputPath = Some buildDir
            MSBuildParams = { p.MSBuildParams with Properties = [("Version", nugetVersion); ("PackageReleaseNotes", packageReleaseNotes)]}
        }
    ) "FSharpLint.sln"
)


Target.create "Pack" (fun _ ->
    let properties = [
        ("Version", nugetVersion);
        ("Authors", authors)
        ("PackageProjectUrl", gitUrl)
        ("RepositoryType", "git")
        ("RepositoryUrl", gitUrl)
        ("PackageLicenseExpression", "MIT")
        ("PackageReleaseNotes", packageReleaseNotes)
    ]


    DotNet.pack (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            OutputPath = Some nugetDir
            MSBuildParams = { p.MSBuildParams with Properties = properties }
        }
    ) "FSharpLint.sln"
)

Target.create "Push" (fun _ ->
    let key =
        match getBuildParam "nuget-key" with
        | s when not (isNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserPassword "NuGet Key: "
    Paket.push (fun p -> { p with WorkingDir = nugetDir; ApiKey = key; ToolType = ToolType.CreateLocalTool() }))

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

Target.runOrDefault "Default"
