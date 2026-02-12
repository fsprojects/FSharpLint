module TestUtils

    open System.IO
    open FSharp.Compiler.CodeAnalysis
    open FSharp.Compiler.Text
    open NUnit.Framework
    open FSharpLint.Framework
    open Utilities

    let private performanceTestSourceFile = 
#if NETCOREAPP2_0
        let basePath = __SOURCE_DIRECTORY__ </>  ".." </> ".."
#else
        let basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".."
#endif
        basePath </> "TypeChecker.fs"

    let asyncGenerateAst source =
        async {
            let checker = FSharpChecker.Create(keepAssemblyContents=true)
            let sourceText = SourceText.ofString source

            let! options = 
                ParseFile.getProjectOptionsFromScript checker performanceTestSourceFile source

            let! parseResults =
                checker.ParseFile(performanceTestSourceFile, sourceText, options |> checker.GetParsingOptionsFromProjectOptions |> fst)

            return parseResults.ParseTree
        }

    let generateAst source =
        asyncGenerateAst source |> Async.RunSynchronously

    let getPerformanceTestInput =
        let memoizedResult = ref None

        let getMemoizedResult () =
            match !memoizedResult with
            | Some(result) -> result
            | None ->
                let text = File.ReadAllText performanceTestSourceFile
                let result = (generateAst text, text)
                memoizedResult := Some(result)
                result

        getMemoizedResult
