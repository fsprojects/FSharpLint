module TestUtils

    open System.IO
    open FSharp.Compiler.SourceCodeServices
    open FSharp.Compiler.Text
    open NUnit.Framework
    open FSharpLint.Framework
    open Utilities

#if NETCOREAPP2_0
    let private basePath = __SOURCE_DIRECTORY__ </>  ".." </> ".."
#else
    let private basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".."
#endif

    let private performanceTestSourceFile = basePath </> "TypeChecker.fs"

    let generateAst source =
        let checker = FSharpChecker.Create()
        let sourceText = SourceText.ofString source

        let options = ParseFile.getProjectOptionsFromScript checker performanceTestSourceFile source

        let parseResults =
            checker.ParseFile(performanceTestSourceFile, sourceText, options |> checker.GetParsingOptionsFromProjectOptions |> fst)
            |> Async.RunSynchronously

        match parseResults.ParseTree with
        | Some(parseTree) -> parseTree
        | None -> failwith "Failed to parse file."

    let getPerformanceTestInput =
        let memoizedResult = ref None

        fun () ->
            match !memoizedResult with
            | Some(result) -> result
            | None ->
                let text = performanceTestSourceFile |> File.ReadAllText
                let result = (generateAst text, text)
                memoizedResult := Some(result)
                result