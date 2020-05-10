namespace FSharpLint.FunctionalTest

module TestApi =

    open System.IO
    open NUnit.Framework
    open FSharpLint.Application.Lint

    let (</>) x y = Path.Combine(x, y)

    let basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".." </> ".."

    let sourceFile = basePath </> "tests" </> "TypeChecker.fs"

    [<TestFixture(Category = "Acceptance Tests")>]
    type TestApi() =

        [<Test>]
        member __.``Lint project via absolute path``() =
            let projectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject"
            let projectFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.fsproj"

            let result = lintProject OptionalLintParameters.Default projectFile

            match result with
            | LintResult.Success warnings ->
                Assert.AreEqual(9, warnings.Length)
            | LintResult.Failure _ ->
                Assert.True(false)

        [<Test>]
        member __.``Lint solution via absolute path``() =
            let projectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject"
            let solutionFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.sln"

            let result = lintSolution OptionalLintParameters.Default solutionFile

            match result with
            | LintResult.Success warnings ->
                Assert.AreEqual(9, warnings.Length)
            | LintResult.Failure _ ->
                Assert.True(false)

        [<Test>]
        member __.``Lint solution with release config``() =
            let projectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject"
            let solutionFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.sln"

            let result = lintSolution { OptionalLintParameters.Default with ReleaseConfiguration = Some "Release" } solutionFile

            match result with
            | LintResult.Success warnings ->
                Assert.AreEqual(9, warnings.Length)
            | LintResult.Failure _ ->
                Assert.True(false)

#if NETCOREAPP // GetRelativePath is netcore-only
        [<Test>]
        member __.``Lint project via relative path``() =
            let projectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject"
            let projectFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.fsproj"

            let relativePathToProjectFile = Path.GetRelativePath (Directory.GetCurrentDirectory(), projectFile)

            let result = lintProject OptionalLintParameters.Default relativePathToProjectFile

            match result with
            | LintResult.Success warnings ->
                Assert.AreEqual(9, warnings.Length)
            | LintResult.Failure _ ->
                Assert.True(false)
            ()

        [<Test>]
        member __.``Lint solution via relative path``() =
            let projectPath = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject"
            let solutionFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.sln"

            let relativePathToSolutionFile = Path.GetRelativePath (Directory.GetCurrentDirectory(), solutionFile)

            let result = lintSolution OptionalLintParameters.Default relativePathToSolutionFile

            match result with
            | LintResult.Success warnings ->
                Assert.AreEqual(9, warnings.Length)
            | LintResult.Failure _ ->
                Assert.True(false)
#endif
