module FSharpLint.Client.Tests

open NUnit.Framework
open System.IO
open System
open Contracts
open LSPFSharpLintService
open LSPFSharpLintServiceTypes

let (</>) x y = Path.Combine(x, y)

let basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".." </> ".."
let fsharpLintConsoleDll = basePath </> "src" </> "FSharpLint.Console" </> "bin" </> "Release" </> "net6.0" </> "dotnet-fsharplint.dll"
let fsharpConsoleOutputDir = Path.GetFullPath (Path.GetDirectoryName(fsharpLintConsoleDll))

let runVersionCall filePath (service: FSharpLintService) =
    async {
        let request = 
            {
                FilePath = filePath
            }
        let! version = service.VersionAsync(request) |> Async.AwaitTask
        return version
    }
    |> Async.RunSynchronously

// ensure current FSharpLint.Console output is in PATH so it can use its daemon if needed
let ensureDaemonPath wantBuiltDaemon =
    let path = Environment.GetEnvironmentVariable("PATH")
    if wantBuiltDaemon then
        if not <| path.Contains(fsharpConsoleOutputDir, StringComparison.InvariantCultureIgnoreCase) then
            Environment.SetEnvironmentVariable("PATH", $"{fsharpConsoleOutputDir}:{path})")
    else if path.Contains(fsharpConsoleOutputDir, StringComparison.InvariantCultureIgnoreCase) then
        Assert.Inconclusive()

[<Test>]
let TestDaemonNotFound() =
    ensureDaemonPath false

    let testHintsFile = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore" </> "TestHints.fs"
    let fsharpLintService: FSharpLintService = new LSPFSharpLintService() :> FSharpLintService
    let versionResponse = runVersionCall testHintsFile fsharpLintService
    
    Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.ToolNotFound, versionResponse.Code)

[<Test>]
let TestDaemonVersion() =
    ensureDaemonPath true

    let testHintsFile = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore" </> "TestHints.fs"
    let fsharpLintService: FSharpLintService = new LSPFSharpLintService() :> FSharpLintService
    let versionResponse = runVersionCall testHintsFile fsharpLintService

    match versionResponse.Result with
    | Content result -> Assert.IsFalse (String.IsNullOrWhiteSpace result)
    // | _ -> Assert.Fail("Response should be a version number")

    Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.Version, versionResponse.Code)

[<Test>]
let TestFilePathShouldBeAbsolute() =
    ensureDaemonPath true

    let testHintsFile = ".." </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore" </> "TestHints.fs"
    let fsharpLintService: FSharpLintService = new LSPFSharpLintService() :> FSharpLintService
    let versionResponse = runVersionCall testHintsFile fsharpLintService
    
    Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.FilePathIsNotAbsolute, versionResponse.Code)

[<Test>]
let TestFileShouldExists() =
    ensureDaemonPath true

    let testHintsFile = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore" </> "TestHintsOOOPS.fs"
    let fsharpLintService: FSharpLintService = new LSPFSharpLintService() :> FSharpLintService
    let versionResponse = runVersionCall testHintsFile fsharpLintService
    
    Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.FileNotFound, versionResponse.Code)
