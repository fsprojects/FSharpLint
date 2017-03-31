module TestUtils

    open System.IO
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open NUnit.Framework

    let (</>) x y = Path.Combine(x, y)

    let private basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".."

    let private performanceTestSourceFile = basePath </> "tests" </> "TypeChecker.fs"

    let generateAst source =
        let checker = FSharpChecker.Create()

        let (options, _diagnostics) = 
            checker.GetProjectOptionsFromScript(performanceTestSourceFile, source) 
            |> Async.RunSynchronously

        let parseResults =
            checker.ParseFileInProject(performanceTestSourceFile, source, options)
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
                let result = generateAst text, text
                memoizedResult := Some(result)
                result