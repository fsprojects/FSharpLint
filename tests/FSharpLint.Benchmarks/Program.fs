open System.IO
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Diagnostics.Windows.Configs
open BenchmarkDotNet.Running
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FSharpLint.Application.Lint

[<EtwProfiler>] // Switch to EventPipeProfiler (cross-platform) after updating to .NET core 3.x
type FSharpLintBenchmark () =

    let generateAst source sourceFile =
        let sourceText = SourceText.ofString source
        let checker = FSharpChecker.Create()

        let (options, _diagnostics) =
            checker.GetProjectOptionsFromScript(sourceFile, sourceText)
            |> Async.RunSynchronously

        let parseResults =
            checker.ParseFile(sourceFile, sourceText, options |> checker.GetParsingOptionsFromProjectOptions |> fst)
            |> Async.RunSynchronously

        match parseResults.ParseTree with
        | Some(parseTree) -> parseTree
        | None -> failwith "Failed to parse file."

    let (</>) x y = Path.Combine(x, y)

    let basePath = ".." </> ".." </> ".." </> ".." </> ".." </> ".." </> ".." </> ".."
    let sourceFile = basePath </> "TypeChecker.fs"

    let fileInfo =
        let text = File.ReadAllText sourceFile
        let tree = generateAst text sourceFile
        { Ast = tree; Source = text; TypeCheckResults = None }

    [<Benchmark>]
    member this.LintParsedFile () =
        lintParsedFile OptionalLintParameters.Default fileInfo sourceFile |> ignore

[<EntryPoint>]
let main _ =
    BenchmarkRunner.Run<FSharpLintBenchmark>() |> ignore
    0
