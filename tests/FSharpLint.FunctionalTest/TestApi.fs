namespace FSharpLint.FunctionalTest

module TestApi =

    open System.IO
    open System.Diagnostics
    open NUnit.Framework
    open FSharpLint.Application.Lint
    open FSharp.Compiler.CodeAnalysis
    open FSharp.Compiler.Text
    open FSharpLint.Framework.Utilities

    let basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".." </> ".."

    let sourceFile = basePath </> "tests" </> "TypeChecker.fs"

    // Test project used for transparent/background compiler project options tests
    let asyncTestProjectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "LibAsync"
    let asyncTestProjectFile = asyncTestProjectPath </> "LibAsync.fsproj"

    [<TestFixture(Category = "Acceptance Tests")>]
    type TestApi() =
        let generateAst source =
            let sourceText = SourceText.ofString source
            let checker = FSharpChecker.Create(keepAssemblyContents=true)

            let (options, _diagnostics) =
                checker.GetProjectOptionsFromScript(sourceFile, sourceText)
                |> Async.RunSynchronously

            let parseResults =
                checker.ParseFile(sourceFile, sourceText, options |> checker.GetParsingOptionsFromProjectOptions |> fst)
                |> Async.RunSynchronously

            parseResults.ParseTree

        /// Must be called once per process.
        let toolsPath = Ionide.ProjInfo.Init.init (DirectoryInfo <| Directory.GetCurrentDirectory())  None

        [<Category("Performance")>]
        [<Test>]
        member _.``Performance of linting an existing file``() =
            let text = File.ReadAllText sourceFile
            let tree = generateAst text
            let fileInfo = { Ast = tree; Source = text; TypeCheckResults = None; ProjectCheckResults = None; ProjectOptions = None }

            let stopwatch = Stopwatch.StartNew()
            let times = ResizeArray()

            let iterations = 100

            for _ in 0..iterations do
                stopwatch.Restart()

                lintParsedFile OptionalLintParameters.Default fileInfo sourceFile |> ignore<LintResult>

                stopwatch.Stop()

                times.Add stopwatch.ElapsedMilliseconds

            let result = times |> Seq.sum |> (fun totalMilliseconds -> totalMilliseconds / int64 iterations)

            Assert.Less(result, 250)
            fprintf TestContext.Out "Average runtime of linter on parsed file: %d (milliseconds)."  result

        // Test linting the async-name test project with the default linting functions, which use the background compiler
        //    This should tokenize "LibAsync.fsproj" tokenizes to ["Lib"; "Async"; ".fsproj"] -> Likely a library.
        [<Test>]
        member _.``Lint async naming test project with background compiler``() =
            task {
                let! result = asyncLintProject OptionalLintParameters.Default asyncTestProjectFile toolsPath

                match result with
                | LintResult.Success warnings ->
                    Assert.AreEqual(1, warnings.Length)
                    Assert.AreEqual(FSharpLint.Rules.Identifiers.AsynchronousFunctionNames, warnings.[0].RuleIdentifier)
                    StringAssert.Contains("This function returns Async. Consider renaming it to AsyncBar.", warnings.[0].Details.Message)
                | LintResult.Failure err ->
                    Assert.Fail(string err)
            }

        [<Test>]
        member _.``Lint project via absolute path``() =
            let projectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore"
            let projectFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.NetCore.fsproj"

            let result = asyncLintProject OptionalLintParameters.Default projectFile toolsPath |> Async.RunSynchronously

            match result with
            | LintResult.Success warnings ->
                Assert.AreEqual(9, warnings.Length)
            | LintResult.Failure err ->
                Assert.True(false, string err)

        [<Test>]
        member _.``Lint multi-targeted project``() =
            let projectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore"
            let projectFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.NetCore.fsproj"

            let result = asyncLintProject OptionalLintParameters.Default projectFile toolsPath |> Async.RunSynchronously

            match result with
            | LintResult.Success warnings ->
                Assert.AreEqual(9, warnings.Length)
            | LintResult.Failure err ->
                Assert.True(false, string err)

        [<Test>]
        member _.``Lint project with default config tries to load fsharplint_json``() =
            let projectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore"
            let projectFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.NetCore.fsproj"
            let tempConfigFile = TestContext.CurrentContext.TestDirectory </> "fsharplint.json"
            File.WriteAllText (tempConfigFile, """{ "ignoreFiles": ["*"] }""")

            let result = asyncLintProject OptionalLintParameters.Default projectFile toolsPath |> Async.RunSynchronously
            File.Delete tempConfigFile

            match result with
            | LintResult.Success warnings ->
                Assert.AreEqual(0, warnings.Length)
            | LintResult.Failure err ->
                Assert.True(false, string err)

        [<TestCase("FSharpLint.FunctionalTest.TestedProject.sln", 18, TestName = "SLN: lint solution (absolute path)")>]
        [<TestCase("FSharpLint.FunctionalTest.TestedProject.slnx", 18, TestName = "SLNX: lint solution (absolute path)")>]
        [<TestCase("FSharpLint.FunctionalTest.TestedProject.slnf", 18, TestName = "SLNF: lint solution (absolute path)")>]
        member _.``Lint solution via absolute path``(solutionFileName: string, expectedWarnings: int) =
            let projectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject"
            let solutionFile = projectPath </> solutionFileName

            let result = asyncLintSolution OptionalLintParameters.Default solutionFile toolsPath |> Async.RunSynchronously

            match result with
            | LintResult.Success warnings ->
                Assert.AreEqual(expectedWarnings, warnings.Length)
            | LintResult.Failure err ->
                Assert.True(false, string err)

#if NETCOREAPP
        [<TestCase("FSharpLint.FunctionalTest.TestedProject.sln", 18, TestName = "SLN: lint solution (relative path)")>]
        [<TestCase("FSharpLint.FunctionalTest.TestedProject.slnx", 18, TestName = "SLNX: lint solution (relative path)")>]
        [<TestCase("FSharpLint.FunctionalTest.TestedProject.slnf", 18, TestName = "SLNF: lint solution (relative path)")>]
        member _.``Lint solution via relative path``(solutionFileName: string, expectedWarnings: int) =
            let projectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject"
            let solutionFile = projectPath </> solutionFileName

            let relativePathToSolutionFile = Path.GetRelativePath (Directory.GetCurrentDirectory(), solutionFile)

            let result =
                asyncLintSolution OptionalLintParameters.Default relativePathToSolutionFile toolsPath
                |> Async.RunSynchronously

            match result with
            | LintResult.Success warnings ->
                Assert.AreEqual(expectedWarnings, warnings.Length)
            | LintResult.Failure err ->
                Assert.True(false, string err)
#endif
