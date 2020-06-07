namespace FSharpLint.Application

open System
open System.Collections.Concurrent
open System.Threading
open FSharp.Compiler.SourceCodeServices
open FSharpLint.Core
open FSharpLint.Framework
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.ParseFile
open FSharpLint.Framework.Rules

/// Provides an API to manage/load FSharpLint configuration files.
/// <see cref="FSharpLint.Framework.Configuration" /> for more information on
/// the default configuration.
module ConfigurationManagement =

    /// Load a FSharpLint configuration file given its contents.
    let loadConfigurationFile (configText:string) =
        parseConfig configText

/// Provides an API for running FSharpLint from within another application.
[<AutoOpen>]
module Lint =

    /// Events which may occur as the linter is running.
    type LintEvent =
        /// A warning has been produced by the linter.
        | ReceivedWarning of Suggestion.LintWarning
        /// Linter started processing a file.
        | StartedLintingFile of fileName : string
        /// A file has completed linting with the provided warnings.
        | FinishedLintingFile of fileName : string * Suggestion.LintWarning list
        /// Linting has failed for the provided file.
        | FailedToLintFile of fileName : string * exn
        /// Linter produced a log message.
        | LogMessage of message : string

    type ConfigurationParam =
        /// Explicit Configuration object to use.
        | Configuration of Configuration
        /// Load configuration from provided file.
        | FromFile of configPath:string
        /// Use default configuration.
        | Default

    /// Reason for the linter failing.
    [<NoComparison>]
    type LintFailure =
        /// The specified file for linting could not be found.
        | FailedToLoadFile of string
        /// Failed to analyse a loaded FSharpLint configuration at runtime e.g. invalid hint.
        | RunTimeConfigError of string
        /// `FSharp.Compiler.Services` failed when trying to parse one or more files.
        | FailedToParseFiles of ParseFile.ParseFileFailure list

    /// Parameters that can be provided to the linter.
    [<NoEquality; NoComparison>]
    type LintParameters = {
        /// Cancels a lint in progress.
        CancellationToken: CancellationToken option
        /// Lint configuration to use.
        Configuration: ConfigurationParam
        /// This function will be called for events which happen as the linter is running.
        HandleLintEvent: (LintEvent -> unit) option
        /// The configuration under which the linter will try to perform parsing.
        ReleaseConfiguration : string option
    } with
        static member Default = {
            LintParameters.CancellationToken = None
            Configuration = Default
            HandleLintEvent = None
            ReleaseConfiguration = None
        }

    /// If your application has already parsed the F# source files using `FSharp.Compiler.Services`
    /// you want to lint then this can be used to provide the parsed information to prevent the
    /// linter from parsing the file again.
    [<NoEquality; NoComparison>]
    type ParsedFileInformation = {
        /// File represented as an AST.
        Ast: FSharp.Compiler.SyntaxTree.ParsedInput
        /// Contents of the file.
        Source: string
        /// Optional results of inferring the types on the AST (allows for a more accurate lint).
        TypeCheckResults: FSharpCheckFileResults option
        /// Path to file for source.
        FilePath : string option
    }

    let private reportLintEvent (handleLintEvent:(LintEvent -> unit) option) (lintEvent:LintEvent) =
        handleLintEvent |> Option.iter (fun handle -> handle lintEvent)

    /// Executes the linter with the provided parameters against the provided file.
    let private lint (lintParams:LintParameters) (config:Configuration) (fileInfo:ParseFile.FileParseInfo) =
        let suggestionsRequiringTypeChecks = ConcurrentStack<_>()

        let fileWarnings = ResizeArray()

        let suggest (warning:Suggestion.LintWarning) =
            LintEvent.ReceivedWarning warning |> reportLintEvent lintParams.HandleLintEvent
            fileWarnings.Add warning

        let trySuggest (suggestion:Suggestion.LintWarning) =
            if suggestion.Details.TypeChecks.IsEmpty then suggest suggestion
            else suggestionsRequiringTypeChecks.Push suggestion

        StartedLintingFile fileInfo.File |> reportLintEvent lintParams.HandleLintEvent

        let cancelHasNotBeenRequested () =
            match lintParams.CancellationToken with
            | Some ct -> not ct.IsCancellationRequested
            | None -> true

        let enabledRules = Configuration.flattenConfig config

        let lines = String.toLines fileInfo.Text |> Array.map (fun (line, _, _) -> line)
        let allRuleNames =
            [|
                enabledRules.LineRules.IndentationRule |> Option.map (fun rule -> rule.Name) |> Option.toArray
                enabledRules.LineRules.NoTabCharactersRule |> Option.map (fun rule -> rule.Name) |> Option.toArray
                enabledRules.LineRules.GenericLineRules |> Array.map (fun rule -> rule.Name)
                enabledRules.AstNodeRules |> Array.map (fun rule -> rule.Name)
            |] |> Array.concat |> Set.ofArray

        let suppressionInfo = Suppression.parseSuppressionInfo allRuleNames lines

        try
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray fileInfo.Ast

            // Collect suggestions for AstNode rules
            let (astNodeSuggestions, context) = LintRunner.runAstNodeRules enabledRules.AstNodeRules enabledRules.GlobalConfig fileInfo.TypeCheckResults fileInfo.File fileInfo.Text lines syntaxArray skipArray
            let lineSuggestions = LintRunner.runLineRules enabledRules.LineRules enabledRules.GlobalConfig fileInfo.File fileInfo.Text lines context

            [| lineSuggestions; astNodeSuggestions |]
            |> Array.concat
            |> Array.filter (fun warning ->
                let line = warning.Details.Range.StartLine
                Suppression.isSuppressed warning.RuleName line suppressionInfo |> not)
            |> Array.iter trySuggest

            if cancelHasNotBeenRequested () then
                let runSynchronously work =
                    let timeoutMs = 2000
                    match lintParams.CancellationToken with
                    | Some cancellationToken -> Async.RunSynchronously(work, timeoutMs, cancellationToken)
                    | None -> Async.RunSynchronously(work, timeoutMs)

                try
                    let typeChecksSuccessful (typeChecks:Async<bool> list) =
                        typeChecks
                        |> List.reduce (Async.combine (&&))

                    let typeCheckSuggestion (suggestion:Suggestion.LintWarning) =
                        typeChecksSuccessful suggestion.Details.TypeChecks
                        |> Async.map (fun checkSuccessful -> if checkSuccessful then Some suggestion else None)

                    suggestionsRequiringTypeChecks
                    |> Seq.map typeCheckSuggestion
                    |> Async.Parallel
                    |> runSynchronously
                    |> Array.iter (function Some suggestion -> suggest suggestion | None -> ())
                with
                | :? TimeoutException -> () // Do nothing.
        with
        | exn -> FailedToLintFile (fileInfo.File, exn) |> reportLintEvent lintParams.HandleLintEvent

        FinishedLintingFile (fileInfo.File, Seq.toList fileWarnings) |> reportLintEvent lintParams.HandleLintEvent

        fileWarnings.ToArray() |> Array.toList

    /// Gets a FSharpLint Configuration based on the provided ConfigurationParam.
    let private getConfig (configParam:ConfigurationParam) =
        match configParam with
        | Configuration config -> Ok config
        | FromFile filePath ->
            try Ok (Configuration.loadConfig filePath)
            with ex -> Error (RunTimeConfigError (string ex))
        | Default -> Ok Configuration.defaultConfiguration

    /// Lints several files given the provided list of file information and lint parameters.
    let private lintFiles (lintParams:LintParameters) (fileInfos:FileParseInfo list) =
        getConfig lintParams.Configuration
        |> Result.map (fun config ->
            let ignoreFiles =
                config.ignoreFiles
                |> Option.map Array.toList
                |> Option.defaultValue []
                |> List.map IgnoreFiles.parseIgnorePath

            fileInfos
            |> List.filter (fun fileInfo -> not (Configuration.IgnoreFiles.shouldFileBeIgnored ignoreFiles fileInfo.File))
            |> List.collect (lint lintParams config))


    /// Lints an entire F# project by retrieving the files from a given path to the `.fsproj` file.
    let lintProject (lintParams:LintParameters) (projectFilePath:string) =
        if IO.File.Exists projectFilePath then
            let checker = FSharpChecker.Create()
            let (projectFiles, projectOptions) = ProjectLoader.getProjectFiles lintParams.ReleaseConfiguration projectFilePath
            let (parsedFiles, errors) =
                projectFiles
                |> List.map (ParseFile.parseFile checker (Some projectOptions))
                |> List.partitionChoices

            if List.isEmpty errors then
                lintFiles lintParams parsedFiles
            else
                Error (FailedToParseFiles errors)
        else
            Error (FailedToLoadFile projectFilePath)

    /// Lints an entire F# solution by linting all projects specified in the `.sln` file.
    let lintSolution (lintParams:LintParameters) (solutionFilePath:string) =
        if IO.File.Exists solutionFilePath then
            let checker = FSharpChecker.Create()

            let (parsedFiles, errors) =
                ProjectLoader.getProjectsFromSolution solutionFilePath
                |> List.collect (fun projectPath ->
                    let (projectFiles, projectOptions) = ProjectLoader.getProjectFiles lintParams.ReleaseConfiguration projectPath
                    projectFiles
                    |> List.map (ParseFile.parseFile checker (Some projectOptions)))
                |> List.partitionChoices

            if List.isEmpty errors then
                lintFiles lintParams parsedFiles
            else
                Error (FailedToParseFiles errors)
        else
            Error (FailedToLoadFile solutionFilePath)

    /// Lints F# source code that has already been parsed using `FSharp.Compiler.Services` in the calling application.
    let lintParsedSource (lintParams:LintParameters) (parsedFileInfo:ParsedFileInformation) =
        let parsedFileInfo =
            { ParseFile.Text = parsedFileInfo.Source
              ParseFile.Ast = parsedFileInfo.Ast
              ParseFile.TypeCheckResults = parsedFileInfo.TypeCheckResults
              ParseFile.File = parsedFileInfo.FilePath |> Option.defaultValue "src.fs" }

        lintFiles lintParams [parsedFileInfo]

    /// Lints F# source code.
    let lintSource (lintParams:LintParameters) (source:string) =
        let checker = FSharpChecker.Create()

        ParseFile.parseSource source checker
        |> Result.mapError (List.singleton >> FailedToParseFiles)
        |> Result.bind (fun parseFileInformation ->
            let parsedFileInfo = {
                FileParseInfo.Text = parseFileInformation.Text
                Ast = parseFileInformation.Ast
                TypeCheckResults = parseFileInformation.TypeCheckResults
                File = "src.fs"
            }
            lintFiles lintParams [parsedFileInfo])

    /// Lints an F# file from a given path to the `.fs` file.
    let lintFile (lintParams:LintParameters) (filePath:string) =
        if IO.File.Exists filePath then
            let checker = FSharpChecker.Create()

            ParseFile.parseFile checker None filePath
            |> Result.mapError (List.singleton >> FailedToParseFiles)
            |> Result.bind (fun astFileParseInfo ->
                let parsedFileInfo = {
                    FileParseInfo.Text = astFileParseInfo.Text
                    Ast = astFileParseInfo.Ast
                    TypeCheckResults = astFileParseInfo.TypeCheckResults
                    File = filePath
                }

                lintFiles lintParams [parsedFileInfo])
        else
            Error (FailedToLoadFile filePath)
