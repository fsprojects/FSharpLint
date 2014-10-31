// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake 
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package 
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project 
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "FSharpLint"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Lint tool for F#."

// List of author names (for NuGet package)
let authors = [ "Matthew Mcveigh" ]

let version = "0.1.6"

// File system information 
// (<solutionFile>.sln is built during the building process)
let solutionFile  = "FSharpLint"
// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin/Release/*Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "https://github.com/fsprojects/FSharpLint"
// The name of the project on GitHub
let gitName = "FSharpLint"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps 
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let fileName = "src/FSharpLint.Console/AssemblyInfo.fs"
  CreateFSharpAssemblyInfo fileName
      [ Attribute.Title project
        Attribute.Product project
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion ] 
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "RestorePackages" RestorePackages

Target "Clean" (fun _ ->
    CleanDirs ["bin"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    !! (solutionFile + ".sln")
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    !! testAssemblies 
    |> NUnit (fun p ->
        { p with
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" })
)

Target "RunFunctionalTests" (fun _ ->
    !! "tests/**/bin/Release/*FunctionalTest*.dll" 
    |> NUnit (fun p ->
        { p with
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" })
)

// --------------------------------------------------------------------------------------
// Create nuget package

Target "CreatePackage" (fun _ ->
    NuGet (fun p -> 
        {p with
            Authors = authors
            Project = project
            Description = summary                               
            OutputPath = "nugetpackage"
            Summary = summary
            WorkingDir = "nugetpackage"
            Version = version
            Publish = false
            Files = 
                [
                    (System.String.Format("build{0}*", System.IO.Path.DirectorySeparatorChar), Some "build", None)
                    (System.String.Format("..{0}src{0}FSharpLint.MSBuildIntegration{0}bin{0}Release{0}*.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                    (System.String.Format("..{0}src{0}FSharpLint.FAKE{0}bin{0}Release{0}FSharpLint.FAKE.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                ]
         })
        "nugetpackage/FSharpLint.nuspec"
)

// --------------------------------------------------------------------------------------
// Generate the documentation web pages

Target "GenerateDocs" (fun _ ->
    executeFSI "docs/tools" "generate.fsx" [] |> ignore
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean" ==> "RestorePackages" ==> "AssemblyInfo" ==> "Build"
"Build" ==> "All"
"RunTests" ==> "RunFunctionalTests" ==> "All"
"GenerateDocs" ==> "All"
"CreatePackage" ==> "All"

RunTargetOrDefault "All"
