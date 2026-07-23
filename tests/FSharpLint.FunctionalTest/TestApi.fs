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
            let fileInfo = { Ast = tree; Source = text; TypeCheckResults = None; ProjectCheckResults = None }

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

        /// Regression: analyzer hosts built on FCS's TransparentCompiler
        /// supply ProjectCheckResults whose FSharpProjectContext.ProjectOptions getter
        /// throws by design. Rules forcing the two-phase ProjectOptions lazy (the library
        /// heuristics in AsynchronousFunctionNames & co.) then failed the whole file with
        /// an internal error. The lazy must degrade to None instead.
        [<Test>]
        member _.``Lint parsed TransparentCompiler results without internal failure``() =
            let source = "module TransparentCompilerRepro\n\nlet answer = async { return 42 }\n"
            let transparentChecker = FSharpChecker.Create(keepAssemblyContents = true, useTransparentCompiler = true)
            let sourceText = SourceText.ofString source

            let (options, _diagnostics) =
                transparentChecker.GetProjectOptionsFromScript(sourceFile, sourceText)
                |> Async.RunSynchronously

            let parseResults, checkAnswer =
                transparentChecker.ParseAndCheckFileInProject(sourceFile, 0, sourceText, options)
                |> Async.RunSynchronously

            let checkResults =
                match checkAnswer with
                | FSharpCheckFileAnswer.Succeeded results -> results
                | FSharpCheckFileAnswer.Aborted -> failwith "type check aborted"

            let projectResults =
                transparentChecker.ParseAndCheckProject options |> Async.RunSynchronously

            let fileInfo =
                { Ast = parseResults.ParseTree
                  Source = source
                  TypeCheckResults = Some checkResults
                  ProjectCheckResults = Some projectResults }

            let mutable internalFailure = None

            let optionalParams =
                { OptionalLintParameters.Default with
                    ReportLinterProgress =
                        Some (fun progress ->
                            match progress with
                            | ProjectProgress.Failed(_, ex) -> internalFailure <- Some ex
                            | _ -> ()) }

            match lintParsedFile optionalParams fileInfo sourceFile with
            | LintResult.Success _ ->
                match internalFailure with
                | Some ex -> Assert.Fail $"lint failed internally: {ex}"
                | None -> ()
            | LintResult.Failure failure -> Assert.Fail(string failure)

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
