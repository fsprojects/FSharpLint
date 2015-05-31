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
let projectApi = "FSharpLintApi"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Lint tool for F#."
let summaryApi = "Lint tool for F#. Api to extend FSharpLint"

// List of author names (for NuGet package)
let authors = [ "Matthew Mcveigh" ]

let version = "0.2.0"
let apiVersion = "0.0.1"

let packagingRoot = "./packaging/"
let toolPackagingDir = packagingRoot @@ "tool"
let apiPackagingDir = packagingRoot @@ "api"

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

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

// Generate assembly info files with the right version & up-to-date information
let genAssemblyInfo (projectPath) =
    let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
    let basePath = "src/" + projectName
    let fileName = basePath + "/AssemblyInfo.fs"
    CreateFSharpAssemblyInfo fileName
      [ Attribute.Title project
        Attribute.Product project
        Attribute.Description summary
        Attribute.Version version
        Attribute.FileVersion version ]

Target "AssemblyInfo" (fun _ ->
    !! "src/**/*.fsproj"
        |> Seq.iter genAssemblyInfo)

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
            OutputPath = packagingRoot
            Summary = summary
            WorkingDir = toolPackagingDir
            Version = version
            Publish = false
            Files = 
                [
                    (System.String.Format("build{0}*", System.IO.Path.DirectorySeparatorChar), Some "build", None)
                    (System.String.Format("..{0}..{0}src{0}FSharpLint.MSBuildIntegration{0}bin{0}Release{0}FSharpLint.MSBuildIntegration.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                    (System.String.Format("..{0}..{0}src{0}FSharpLint.Worker{0}bin{0}Release{0}FSharpLint.Worker.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                    (System.String.Format("..{0}..{0}src{0}FSharpLint.FAKE{0}bin{0}Release{0}FSharpLint.FAKE.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                    (System.String.Format("..{0}..{0}src{0}FSharpLint.CrossDomain{0}bin{0}Release{0}FSharpLint.CrossDomain.dll", System.IO.Path.DirectorySeparatorChar), None, None)

                    (System.String.Format("..{0}..{0}bin{0}FSharpLint.Rules.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                    (System.String.Format("..{0}..{0}bin{0}FSharpLint.Framework.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                    (System.String.Format("..{0}..{0}bin{0}FSharpLint.Application.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                    (System.String.Format("..{0}..{0}bin{0}FSharp.Compiler.Service.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                    (System.String.Format("..{0}..{0}bin{0}FParsecCS.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                    (System.String.Format("..{0}..{0}bin{0}FParsec.dll", System.IO.Path.DirectorySeparatorChar), None, None)
                ]
         })
        "FSharpLint.nuspec"
)

Target "CreateApiPackage" (fun _ ->
    let libDir = apiPackagingDir @@ "lib"
    CleanDirs [libDir]

    CopyFile libDir "./bin/FSharpLint.Rules.dll"
    CopyFile libDir "./bin/FSharpLint.Framework.dll"
    CopyFile libDir "./bin/FSharpLint.Application.dll"
    CopyFile libDir "./bin/FSharp.Compiler.Service.dll"
    CopyFile libDir "./bin/FParsecCS.dll"
    CopyFile libDir "./bin/FParsec.dll"

    NuGet (fun p -> 
        { p with
            Authors = authors
            Project = projectApi
            Description = summaryApi
            OutputPath = packagingRoot
            Summary = summaryApi
            WorkingDir = apiPackagingDir
            Version = apiVersion
            Publish = false
         })
        "FSharpLintApi.nuspec"
)

#I @"tools/FSharpLint.0.2.0/"
#r @"tools/FSharpLint.0.2.0/FSharpLint.FAKE.dll"
open FSharpLint.FAKE

Target "Lint" (fun _ ->
    !! "src/**/*.fsproj"
        |> Seq.iter (FSharpLint id))

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
"RunFunctionalTests" ==> "RunTests" ==> "All"
"Lint" ==> "All"
"GenerateDocs" ==> "All"
"CreatePackage" ==> "All"
"CreateApiPackage" ==> "All"

RunTargetOrDefault "All"