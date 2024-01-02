module FSharpLint.Client.Tests

open NUnit.Framework
open System.IO
open System
open Contracts
open LSPFSharpLintService
open LSPFSharpLintServiceTypes

let (</>) path1 path2 = Path.Combine(path1, path2)

let basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".." </> ".."
let fsharpLintConsoleDll = basePath </> "src" </> "FSharpLint.Console" </> "bin" </> "Release" </> "net9.0" </> "dotnet-fsharplint.dll"
let fsharpConsoleOutputDir = Path.GetFullPath (Path.GetDirectoryName(fsharpLintConsoleDll))

[<RequireQualifiedAccess>]
type ToolStatus = | Available | NotAvailable
type ToolLocationOverride(toolStatus: ToolStatus) =
    let tempFolder = Path.GetTempFileName()

    do match toolStatus with
       | ToolStatus.Available -> Environment.SetEnvironmentVariable("FSHARPLINT_SEARCH_PATH_OVERRIDE", fsharpConsoleOutputDir)
       | ToolStatus.NotAvailable ->
            let path = Environment.GetEnvironmentVariable("PATH")
            // ensure bin dir is not in path
            if path.Contains(fsharpConsoleOutputDir, StringComparison.InvariantCultureIgnoreCase) then
                Assert.Inconclusive()

            File.Delete(tempFolder)
            Directory.CreateDirectory(tempFolder) |> ignore<DirectoryInfo>

            // set search path to an empty dir
            Environment.SetEnvironmentVariable("FSHARPLINT_SEARCH_PATH_OVERRIDE", tempFolder)

    interface IDisposable with
        member this.Dispose() =
            if File.Exists tempFolder then
                File.Delete tempFolder

let runVersionCall filePath (service: IFSharpLintService) =
    async {
        let request =
            {
                FilePath = filePath
            }
        let! version = service.VersionAsync(request) |> Async.AwaitTask
        return version
    }
    |> Async.RunSynchronously

let runLintFileCall filePath (service: FSharpLintService) =
    async {
        let request = 
            {
                FilePath = filePath
                LintConfigPath = None
            }
        let! lintResult = service.LintFileAsync(request) |> Async.AwaitTask
        return lintResult
    }
    |> Async.RunSynchronously

[<Test>]
let ``Daemon cannot be found``() =
    using (new ToolLocationOverride(ToolStatus.NotAvailable)) <| fun _ ->

        let testHintsFile = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore" </> "TestHints.fs"
        let fsharpLintService: IFSharpLintService = new LSPFSharpLintService() :> IFSharpLintService
        let versionResponse = runVersionCall testHintsFile fsharpLintService

        Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.ErrToolNotFound, versionResponse.Code)

[<Test>]
let ``Daemon answer with its version number``() =
    using (new ToolLocationOverride(ToolStatus.Available)) <| fun _ ->

        let testHintsFile = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore" </> "TestHints.fs"
        let fsharpLintService: IFSharpLintService = new LSPFSharpLintService() :> IFSharpLintService
        let versionResponse = runVersionCall testHintsFile fsharpLintService

        match versionResponse.Result with
        | Content result -> Assert.IsFalse (String.IsNullOrWhiteSpace result)
        | _ -> Assert.Fail("Response should be a version number")

        Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.OkCurrentDaemonVersion, versionResponse.Code)

[<Test>]
let ``Daemon cannot work with relative path``() =
    using (new ToolLocationOverride(ToolStatus.Available)) <| fun _ ->

        let testHintsFile = ".." </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore" </> "TestHints.fs"
        let fsharpLintService: IFSharpLintService = new LSPFSharpLintService() :> IFSharpLintService
        let versionResponse = runVersionCall testHintsFile fsharpLintService

        Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.ErrFilePathIsNotAbsolute, versionResponse.Code)

[<Test>]
let ``Daemon cannot work with non-existing file``() =
    using (new ToolLocationOverride(ToolStatus.Available)) <| fun _ ->

        let testHintsFile = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore" </> "TestHintsOOOPS.fs"
        let fsharpLintService: IFSharpLintService = new LSPFSharpLintService() :> IFSharpLintService
        let versionResponse = runVersionCall testHintsFile fsharpLintService

        Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.ErrFileNotFound, versionResponse.Code)

[<Test>]
let ``Daemon can lint a file with success``() =
    using (new ToolLocationOverride(ToolStatus.Available)) <| fun _ ->

        let testHintsFile = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore" </> "TestHints.fs"
        let fsharpLintService: FSharpLintService = new LSPFSharpLintService() :> FSharpLintService
        let versionResponse = runLintFileCall testHintsFile fsharpLintService

        match versionResponse.Result with
        | Content result -> Assert.Fail("Should be a lint result")
        | LintResult warnings -> 
            Assert.IsNotEmpty warnings

            warnings
            |> List.iter(fun warning ->
                Assert.IsNotEmpty warning.RuleName
                Assert.IsTrue (warning.RuleIdentifier.StartsWith("FL"))
                Assert.IsNotEmpty warning.ErrorText
                Assert.IsNotEmpty warning.FilePath

                Assert.IsNotEmpty warning.Details.Message
                Assert.Positive warning.Details.Range.StartLine
                Assert.Positive warning.Details.Range.StartColumn
                Assert.Positive warning.Details.Range.EndLine
                Assert.Positive warning.Details.Range.EndColumn
                Assert.IsTrue <| Option.isSome warning.Details.SuggestedFix)

            Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.OkLint, versionResponse.Code)

[<Test; Ignore("not sure how to make file parsing fail")>]
let ``LintError if Daemon lint an unparsable file``() =
    using (new ToolLocationOverride(ToolStatus.Available)) <| fun _ ->

        let testHintsFile = basePath </> "tests" </> "FSharpLint.Client.Tests" </> "UnparsableFile.fs"
        let fsharpLintService: FSharpLintService = new LSPFSharpLintService() :> FSharpLintService
        let versionResponse = runLintFileCall testHintsFile fsharpLintService

        match versionResponse.Result with
        | Content result -> Assert.Fail("Should be a lint result")
        | LintResult warnings -> 
            Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.OkLintError, versionResponse.Code)
            Assert.IsEmpty warnings
