/// Provides functionality to parse F# files using `FSharp.Compiler.Service`.
module FSharpLint.Framework.ParseFile

open System.IO
open FSharpLint.Core
open FSharpLint.Framework
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Utilities

/// Information for a file to be linted that is given to the analysers.
[<NoEquality; NoComparison>]
type internal FileParseInfo = {
    /// Contents of the file.
    Text: string
    /// File represented as an AST.
    Ast: ParsedInput
    /// Optional results of inferring the types on the AST (allows for a more accurate lint).
    TypeCheckResults: FSharpCheckFileResults option
    /// Path to the file.
    File: string
}

[<NoComparison>]
type ParseFileFailure =
    | FailedToParseFile of FSharpErrorInfo []
    | AbortedTypeCheck

let private parse file source (checker:FSharpChecker, options) =
    let sourceText = SourceText.ofString source
    let parseResults =
        checker.ParseFile(file, sourceText, options |> checker.GetParsingOptionsFromProjectOptions |> fst)
        |> Async.RunSynchronously

    let typeCheckFile () =
        let results =
            checker.CheckFileInProject(parseResults, file, 0, sourceText, options)
            |> Async.RunSynchronously

        match results with
        | FSharpCheckFileAnswer.Succeeded x -> Ok x
        | FSharpCheckFileAnswer.Aborted -> Error AbortedTypeCheck

    parseResults.ParseTree
    |> Result.ofOption (FailedToParseFile parseResults.Errors)
    |> Result.bind (fun parseTree ->
        typeCheckFile ()
        |> Result.map (fun typeCheckResults ->
            { Text = source
              Ast = parseTree
              TypeCheckResults = Some typeCheckResults
              File = file })
        |> Result.mapError (fun _ -> AbortedTypeCheck))

// See: https://github.com/fsharp/FSharp.Compiler.Service/issues/847.
let private dotnetCoreReferences () =
    let fsharpCoreDir = Path.GetDirectoryName(typeof<FSharp.Collections.List<_>>.Assembly.Location)
    let runtimeDir = Path.GetDirectoryName(typeof<System.Object>.Assembly.Location)

    [ fsharpCoreDir </> "FSharp.Core.dll"
      runtimeDir </> "mscorlib.dll"
      runtimeDir </> "System.Console.dll"
      runtimeDir </> "System.Runtime.dll"
      runtimeDir </> "System.Private.CoreLib.dll"
      runtimeDir </> "System.ObjectModel.dll"
      runtimeDir </> "System.IO.dll"
      runtimeDir </> "System.Linq.dll"
      runtimeDir </> "System.Net.Requests.dll"
      runtimeDir </> "System.Runtime.Numerics.dll"
      runtimeDir </> "System.Threading.Tasks.dll"

      typeof<System.Console>.Assembly.Location
      typeof<System.ComponentModel.DefaultValueAttribute>.Assembly.Location
      typeof<System.ComponentModel.PropertyChangedEventArgs>.Assembly.Location
      typeof<System.IO.BufferedStream>.Assembly.Location
      typeof<System.Linq.Enumerable>.Assembly.Location
      typeof<System.Net.WebRequest>.Assembly.Location
      typeof<System.Numerics.BigInteger>.Assembly.Location
      typeof<System.Threading.Tasks.TaskExtensions>.Assembly.Location ]
    |> List.distinct
    |> List.filter File.Exists
    |> List.distinctBy Path.GetFileName
    |> List.map (fun location -> "-r:" + location)

let internal getProjectOptionsFromScript (checker:FSharpChecker) file (source:string) =
    let sourceText = SourceText.ofString source
    #if NETSTANDARD2_0
    let assumeDotNetFramework = false
    let useSdkRefs = true
    let targetProfile = "--targetProfile:netstandard"
    #else
    let assumeDotNetFramework = true
    let useSdkRefs = false
    let targetProfile = "--targetProfile:mscorlib"
    #endif

    let (options, _diagnostics) =
        checker.GetProjectOptionsFromScript(file, sourceText, assumeDotNetFramework = assumeDotNetFramework, useSdkRefs = useSdkRefs, otherFlags = [| targetProfile |])
        |> Async.RunSynchronously

    let otherOptions =
        if assumeDotNetFramework then options.OtherOptions
        else
            [| yield! options.OtherOptions |> Array.filter (fun x -> not (x.StartsWith("-r:")))
               yield! dotnetCoreReferences() |]

    { options with OtherOptions = otherOptions }

/// Parses a file using `FSharp.Compiler.Service`.
let internal parseFile (checker:FSharpChecker) projectOptions filePath =
    let source = File.ReadAllText(filePath)

    let projectOptions =
        match projectOptions with
        | Some existingOptions -> existingOptions
        | None -> getProjectOptionsFromScript checker filePath source

    parse filePath source (checker, projectOptions)

/// Parses source code using `FSharp.Compiler.Service`.
let internal parseSource (source:string) (checker:FSharpChecker) =
    let options = getProjectOptionsFromScript checker "src.fs" source
    parse "src.fs" source (checker, options)
