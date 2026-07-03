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

        /// Parse + check `source` under FCS's TransparentCompiler (as an analyzer host would),
        /// returning the parse tree together with the file and project check results.
        let checkSourceUnderTransparentCompiler source =
            let checker = FSharpChecker.Create(keepAssemblyContents = true, useTransparentCompiler = true)
            let sourceText = SourceText.ofString source
            let (options, _diagnostics) =
                checker.GetProjectOptionsFromScript(sourceFile, sourceText) |> Async.RunSynchronously
            let parseResults, checkAnswer =
                checker.ParseAndCheckFileInProject(sourceFile, 0, sourceText, options) |> Async.RunSynchronously
            let checkResults =
                match checkAnswer with
                | FSharpCheckFileAnswer.Succeeded results -> results
                | FSharpCheckFileAnswer.Aborted -> failwith "type check aborted"
            let projectResults = checker.ParseAndCheckProject options |> Async.RunSynchronously
            (parseResults.ParseTree, checkResults, projectResults)

        [<Category("Performance")>]
        [<Test>]
        member _.``Performance of linting an existing file``() =
            let text = File.ReadAllText sourceFile
            let tree = generateAst text
            let fileInfo = { Ast = tree; Source = text; TypeCheckResults = None; ProjectCheckResults = None; ProjectFileName = None }

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

        /// Regression: analyzer hosts built on FCS's TransparentCompiler supply
        /// ProjectCheckResults whose FSharpProjectContext.ProjectOptions getter throws by
        /// design. Deriving the project file name from them must degrade to None (guarded)
        /// rather than failing the whole file with an internal error.
        [<Test>]
        member _.``Lint parsed TransparentCompiler results without internal failure``() =
            let source = "module TransparentCompilerRepro\n\nlet answer = async { return 42 }\n"
            let parseTree, checkResults, projectResults = checkSourceUnderTransparentCompiler source

            let fileInfo =
                { Ast = parseTree
                  Source = source
                  TypeCheckResults = Some checkResults
                  ProjectCheckResults = Some projectResults
                  ProjectFileName = None }

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

        /// The recovery this branch adds: a caller (e.g. an analyzer host) that knows the
        /// project file supplies it directly via ProjectFileName, so the library-heuristic
        /// rules keep working under the TransparentCompiler where the project options
        /// cannot be derived from the check results.
        [<Test>]
        member _.``Caller-supplied ProjectFileName drives the library heuristic under TransparentCompiler``() =
            // A public function returning Async<'T>: AsynchronousFunctionNames (default mode
            // OnlyPublicAPIsInLibraries) flags it only when the project looks like a library.
            let source = "module Foo =\n    let Bar(): Async<int> =\n        async { return 1 }\n"
            let parseTree, checkResults, projectResults = checkSourceUnderTransparentCompiler source

            let flagsBar projectFileName =
                let fileInfo =
                    { Ast = parseTree
                      Source = source
                      TypeCheckResults = Some checkResults
                      ProjectCheckResults = Some projectResults
                      ProjectFileName = projectFileName }
                match lintParsedFile OptionalLintParameters.Default fileInfo sourceFile with
                | LintResult.Success warnings ->
                    warnings |> List.exists (fun warning -> warning.Details.Message.Contains "AsyncBar")
                | LintResult.Failure failure -> failwith (string failure)

            // "MyLib.fsproj" tokenises to ["My"; "Lib"; ".fsproj"] -> Likely a library.
            Assert.IsTrue(
                flagsBar (Some "MyLib.fsproj"),
                "caller-supplied library project name should engage the async-naming rule under TransparentCompiler")
            // With no project file name the derivation is impossible under TC -> degrade to not-a-library.
            Assert.IsFalse(
                flagsBar None,
                "with no project file name the library-gated rule should not fire")

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
