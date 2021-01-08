namespace FSharpLint.Benchmarks

open System.IO
open BenchmarkDotNet.Attributes
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FSharpLint.Application.Lint
open FSharpLint.Framework

type Benchmark () =

    let generateAst source sourceFile =
        let sourceText = SourceText.ofString source
        let checker = FSharpChecker.Create(keepAssemblyContents=true)

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

    let (fileInfo, lines) =
        let text = File.ReadAllText sourceFile
        let tree = generateAst text sourceFile
        ({ Ast = tree; Source = text; TypeCheckResults = None }, String.getLines text |> Array.toList)

    [<Benchmark>]
    member this.LintParsedFile () =
        lintParsedFile OptionalLintParameters.Default fileInfo sourceFile |> ignore
