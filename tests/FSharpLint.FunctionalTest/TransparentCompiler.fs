namespace FSharpLint.FunctionalTest

#nowarn "57" // 'FSharpProjectSnapshot' is considered experimental. Note: Could suppress this more locally if building with the .NET 10 compiler

module TestApiWithTransparentCompiler =

    open System.IO
    open NUnit.Framework
    open FSharpLint.Application.Lint
    open FSharp.Compiler.CodeAnalysis
    open FSharp.Compiler.Text
    open FSharpLint.Framework.Utilities
    open FSharpLint.Framework

    let testSourceFilePath = Path.GetFullPath(TestApi.asyncTestProjectPath </> "LibAsyncNames.fs")

    [<TestFixture(Category = "Transparent Compiler Tests")>]
    type TestApiWithTransparentCompiler() =

        /// Parse + check `source` under FCS's TransparentCompiler (as an analyzer host would),
        /// returning the parse tree together with the file and project check results.
        let checkSourceUnderTransparentCompiler sourceFile source  =
            async {
                let checker = FSharpChecker.Create(keepAssemblyContents = true, useTransparentCompiler = true)
                let sourceText = SourceTextNew.ofString source
                let! (snapShot, _diagnostics) =
                    checker.GetProjectSnapshotFromScript(sourceFile, sourceText)
                let! parseResults, checkAnswer =
                    checker.ParseAndCheckFileInProject(sourceFile, snapShot)
                let checkResults =
                    match checkAnswer with
                    | FSharpCheckFileAnswer.Succeeded results -> results
                    | FSharpCheckFileAnswer.Aborted -> failwith "type check aborted"
                let! projectResults = checker.ParseAndCheckProject snapShot
                return (parseResults.ParseTree, checkResults, projectResults, snapShot)
            }

        // Test linting the async-name test project project machinery using the transparent compiler
        //    This should tokenize "LibAsync.fsproj" tokenizes to ["Lib"; "Async"; ".fsproj"] -> Likely a library.
        [<Test>]
        member _.``Lint async naming test project with transparent compiler``() =
            // A public function returning Async<'T>: AsynchronousFunctionNames (default mode
            // OnlyPublicAPIsInLibraries) flags it only when the project looks like a library.
            let source  = File.ReadAllText(testSourceFilePath)

            task {

                let! parseTree, checkResults, projectResults, snapshot = 
                    checkSourceUnderTransparentCompiler testSourceFilePath source

                let fileInfo =
                    { Ast = parseTree
                      Source = File.ReadAllText testSourceFilePath
                      TypeCheckResults = Some checkResults
                      ProjectCheckResults = Some projectResults
                      ProjectOptions = Some (ParseFile.LinterProjectOptions.TransparentCompilerOptions snapshot) }

                match lintParsedFile OptionalLintParameters.Default fileInfo testSourceFilePath with
                | LintResult.Success warnings ->
                    Assert.AreEqual(1, warnings.Length)
                    Assert.AreEqual(FSharpLint.Rules.Identifiers.AsynchronousFunctionNames, warnings.[0].RuleIdentifier)
                    StringAssert.Contains("This function returns Async. Consider renaming it to AsyncBar.", warnings.[0].Details.Message)
                | LintResult.Failure failure -> 
                    Assert.Fail (string failure)

            }

        // Doing a lint with no project options should mean that the project name based rules won't fire, *but* we shouldn't get any errors
        // as a result of the missing optional properties
        [<Test>]
        member _.``Lint async naming test project with transparent compiler with no project options``() =
            // A public function returning Async<'T>: AsynchronousFunctionNames (default mode
            // OnlyPublicAPIsInLibraries) flags it only when the project looks like a library.
            let source  = File.ReadAllText(testSourceFilePath)

            task {

                let! parseTree, checkResults, projectResults, _ = 
                    checkSourceUnderTransparentCompiler testSourceFilePath source

                let fileInfo =
                    { Ast = parseTree
                      Source = File.ReadAllText testSourceFilePath
                      TypeCheckResults = Some checkResults
                      ProjectCheckResults = Some projectResults
                      ProjectOptions = None }

                match lintParsedFile OptionalLintParameters.Default fileInfo testSourceFilePath with
                | LintResult.Success warnings ->
                    Assert.IsEmpty warnings 
                | LintResult.Failure failure -> 
                    Assert.Fail (string failure)
            }
