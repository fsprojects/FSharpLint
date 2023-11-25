namespace FSharpLint.Framework

/// Provides functionality to parse F# files using `FSharp.Compiler.Service`.
module ParseFile =

    open System.IO
    open FSharpLint.Framework
    open FSharp.Compiler.CodeAnalysis
    open FSharp.Compiler.Diagnostics
    open FSharp.Compiler.Syntax
    open FSharp.Compiler.Text
    open Utilities

    /// Information for a file to be linted that is given to the analysers.
    [<NoEquality; NoComparison>]
    type FileParseInfo = {
        /// Contents of the file.
        Text:string

        /// File represented as an AST.
        Ast:ParsedInput

        /// Optional results of inferring the types on the AST (allows for a more accurate lint).
        TypeCheckResults:FSharpCheckFileResults option

        /// Path to the file.
        File:string
    }

    [<NoComparison>]
    type ParseFileFailure =
        | FailedToParseFile of FSharpDiagnostic []
        | AbortedTypeCheck

    [<NoComparison>]
    type ParseFileResult<'T> =
        | Failed of ParseFileFailure
        | Success of 'T

    let private parse file source (checker:FSharpChecker, options) =
        let sourceText = SourceText.ofString source
        let (parseResults, checkFileAnswer) =
            checker.ParseAndCheckFileInProject(file, 0, sourceText, options)
            |> Async.RunSynchronously

        match checkFileAnswer with
        | FSharpCheckFileAnswer.Succeeded(typeCheckResults) ->
            { Text = source
              Ast = parseResults.ParseTree
              TypeCheckResults = Some(typeCheckResults)
              File = file } |> Success
        | FSharpCheckFileAnswer.Aborted -> Failed(AbortedTypeCheck)

    let getProjectOptionsFromScript (checker:FSharpChecker) file (source:string) =
        let sourceText = SourceText.ofString source
        let assumeDotNetFramework = false
        let otherOpts = [| "--targetprofile:netstandard" |]

        let (options, _diagnostics) =
            checker.GetProjectOptionsFromScript(file, sourceText, assumeDotNetFramework = assumeDotNetFramework, useSdkRefs = not assumeDotNetFramework, otherFlags = otherOpts)
            |> Async.RunSynchronously
        options

    /// Parses a file using `FSharp.Compiler.Service`.
    let parseFile file (checker:FSharpChecker) projectOptions =
        let source = File.ReadAllText(file)

        let projectOptions =
            match projectOptions with
            | Some(existingOptions) -> existingOptions
            | None -> getProjectOptionsFromScript checker file source

        parse file source (checker, projectOptions)

    /// Parses source code using `FSharp.Compiler.Service`.
    let parseSourceFile fileName source (checker:FSharpChecker) =
        let options = getProjectOptionsFromScript checker fileName source

        parse fileName source (checker, options)

    let parseSource source (checker:FSharpChecker) =
        let fileName = Path.ChangeExtension(Path.GetTempFileName(), "fsx")
        parseSourceFile fileName source checker
