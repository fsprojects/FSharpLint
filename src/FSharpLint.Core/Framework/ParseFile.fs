namespace FSharpLint.Framework

#nowarn "FS0057" // 'FSharpProjectSnapshot' is considered experimental. Note: Could suppress this more locally if building with the .NET 10 compiler

/// Provides functionality to parse F# files using `FSharp.Compiler.Service`.
module ParseFile =

    open System.IO
    open FSharpLint.Framework
    open FSharp.Compiler.CodeAnalysis
    open FSharp.Compiler.Diagnostics
    open FSharp.Compiler.Syntax
    open FSharp.Compiler.Text
    open Utilities

    /// Options related to the project being linted.
    /// Based on https://github.com/ionide/FSharp.Analyzers.SDK/blob/f323144f0a4db51be564a3187838f2328f0e9182/src/FSharp.Analyzers.SDK/FSharp.Analyzers.SDK.fsi#L66
    [<NoEquality; NoComparison>]
    type LinterProjectOptions =
        | BackgroundCompilerOptions of options: FSharpProjectOptions
        | TransparentCompilerOptions of snapshot: FSharpProjectSnapshot

        member this.ProjectFileName =
            match this with
            | BackgroundCompilerOptions(options) -> options.ProjectFileName
            | TransparentCompilerOptions(snapshot) -> snapshot.ProjectFileName

#if false
        member x.ProjectId =
            match x with
            | BackgroundCompilerOptions(options) -> options.ProjectId
            | TransparentCompilerOptions(snapshot) -> snapshot.ProjectId

        member x.SourceFiles =
            match x with
            | BackgroundCompilerOptions(options) ->
                options.SourceFiles
                |> Array.toList
            | TransparentCompilerOptions(snapshot) ->
                snapshot.SourceFiles
                |> List.map (fun f -> f.FileName)
            |> List.map System.IO.Path.GetFullPath

        member x.ReferencedProjectsPath =
            match x with
            | BackgroundCompilerOptions(options) ->
                options.ReferencedProjects
                |> Array.choose (fun p -> p.ProjectFilePath)
                |> Array.toList
            | TransparentCompilerOptions(snapshot) ->
                snapshot.ReferencedProjects
                |> List.choose (fun p -> p.ProjectFilePath)

        member x.LoadTime =
            match x with
            | BackgroundCompilerOptions(options) -> options.LoadTime
            | TransparentCompilerOptions(snapshot) -> snapshot.LoadTime

        member x.OtherOptions =
            match x with
            | BackgroundCompilerOptions(options) ->
                options.OtherOptions
                |> Array.toList
            | TransparentCompilerOptions(snapshot) -> snapshot.OtherOptions

#endif

    /// Information for a file to be linted that is given to the analysers.
    [<NoEquality; NoComparison>]
    type FileParseInfo = {
        /// Contents of the file.
        Text:string

        /// File represented as an AST.
        Ast:ParsedInput

        /// Optional results of inferring the types on the AST (allows for a more accurate lint).
        TypeCheckResults:FSharpCheckFileResults option

        /// Optional results of project-wide type info (allows for a more accurate lint).
        ProjectCheckResults:FSharpCheckProjectResults option

        /// Optional project options. Allows rules to operate on project options.
        ProjectOptions: LinterProjectOptions option

        /// Path to the file.
        File:string
    }

    [<NoComparison>]
    type ParseFileFailure =
        | FailedToParseFile of failures: FSharpDiagnostic []
        | AbortedTypeCheck

    [<NoComparison>]
    type ParseFileResult<'Content> =
        | Failed of failure: ParseFileFailure
        | Success of result: 'Content

    let private parse file source (checker:FSharpChecker, options) = async {
        let sourceText = SourceText.ofString source
        let! parseResults, checkFileAnswer =
            checker.ParseAndCheckFileInProject(file, 0, sourceText, options)

        match checkFileAnswer with
        | FSharpCheckFileAnswer.Succeeded(typeCheckResults) ->
            return Success
                {
                    Text = source
                    Ast = parseResults.ParseTree
                    TypeCheckResults = Some(typeCheckResults)
                    ProjectCheckResults = None
                    ProjectOptions =  Some (BackgroundCompilerOptions options) 
                    File = file
                }
        | FSharpCheckFileAnswer.Aborted -> return Failed(AbortedTypeCheck)
    }

    let getProjectOptionsFromScript (checker:FSharpChecker) file (source:string) = async {
        let sourceText = SourceText.ofString source
        let assumeDotNetFramework = false
        let otherOpts = Array.singleton "--targetprofile:netstandard"

        let! options, _diagnostics =
            checker.GetProjectOptionsFromScript(file, sourceText, assumeDotNetFramework = assumeDotNetFramework, useSdkRefs = not assumeDotNetFramework, otherFlags = otherOpts)
        return options
    }

    /// Parses a file using `FSharp.Compiler.Service`.
    let parseFile file (checker:FSharpChecker) projectOptions = async {
        let source = File.ReadAllText(file)

        let! projectOptions =
            match projectOptions with
            | Some(existingOptions) -> async { return existingOptions }
            | None -> getProjectOptionsFromScript checker file source

        return! parse file source (checker, projectOptions)
    }

    /// Parses source code using `FSharp.Compiler.Service`.
    let parseSourceFile fileName source (checker:FSharpChecker) = async {
        let! options = getProjectOptionsFromScript checker fileName source

        return! parse fileName source (checker, options)
    }

    let parseSource source (checker:FSharpChecker) =
        let fileName = Path.ChangeExtension(Path.GetTempFileName(), "fsx")
        parseSourceFile fileName source checker
