namespace FSharpLint.Application

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open System.Threading
open FSharp.Compiler.Text
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.CodeAnalysis
open Microsoft.Build.Construction
open Ionide.ProjInfo.ProjectSystem
open Ionide.ProjInfo.FCS
open FSharpLint.Core
open FSharpLint.Framework
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.Rules
open FSharpLint.Rules

/// Provides an API to manage/load FSharpLint configuration files.
/// <see cref="FSharpLint.Framework.Configuration" /> for more information on
/// the default configuration.
module ConfigurationManagement =

    /// Load a FSharpLint configuration file given its contents.
    let loadConfigurationFile (configurationFileText:string) =
        parseConfig configurationFileText

/// Provides an API for running FSharpLint from within another application.
[<AutoOpen>]
module Lint =

    type BuildFailure = | InvalidProjectFileMessage of string

    /// Reason for the linter failing.
    [<NoComparison>]
    type LintFailure =
        /// Project file path did not exist on the local filesystem.
        | ProjectFileCouldNotBeFound of string

        /// Received exception when trying to get the list of F# file from the project file.
        | MSBuildFailedToLoadProjectFile of string * BuildFailure

        /// Failed to load a FSharpLint configuration file.
        | FailedToLoadConfig of string

        /// The specified file for linting could not be found.
        | FailedToLoadFile of string

        /// Failed to analyse a loaded FSharpLint configuration at runtime e.g. invalid hint.
        | RunTimeConfigError of string

        /// `FSharp.Compiler.Services` failed when trying to parse a file.
        | FailedToParseFile of ParseFile.ParseFileFailure

        /// `FSharp.Compiler.Services` failed when trying to parse one or more files in a project.
        | FailedToParseFilesInProject of ParseFile.ParseFileFailure list

        /// Failed to infer input type from target
        | FailedToInferInputType of string

        member this.Description
            with get() =
                let getParseFailureReason = function
                    | ParseFile.FailedToParseFile failures ->
                        let getFailureReason (fSharpDiagnostic:FSharpDiagnostic) =
                            $"failed to parse file {fSharpDiagnostic.FileName}, message: {fSharpDiagnostic.Message}"

                        String.Join(", ", Array.map getFailureReason failures)
                    | ParseFile.AbortedTypeCheck -> "Type check failed. You might want to build your solution/project first and try again."

                match this with
                | ProjectFileCouldNotBeFound projectPath ->
                    String.Format(Resources.GetString("ConsoleProjectFileCouldNotBeFound"), projectPath)
                | MSBuildFailedToLoadProjectFile (projectPath, message) ->
                    String.Format(Resources.GetString("ConsoleMSBuildFailedToLoadProjectFile"), projectPath, message)
                | FailedToLoadFile filepath ->
                    String.Format(Resources.GetString("ConsoleCouldNotFindFile"), filepath)
                | FailedToLoadConfig message ->
                    String.Format(Resources.GetString("ConsoleFailedToLoadConfig"), message)
                | RunTimeConfigError error ->
                    String.Format(Resources.GetString("ConsoleRunTimeConfigError"), error)
                | FailedToParseFile failure ->
                    $"Lint failed to parse a file. Failed with: {getParseFailureReason failure}"
                | FailedToParseFilesInProject failures ->
                    let failureReasons = String.Join("\n", failures |> List.map getParseFailureReason)
                    $"Lint failed to parse files. Failed with: {failureReasons}"
                | FailedToInferInputType target ->
                    $"Input type could not be inferred from target '{target}'. Explicitly set input type using --file-type parameter."

    [<NoComparison>]
    type Result<'SuccessType> =
        | Success of 'SuccessType
        | Failure of LintFailure

    /// Provides information on what the linter is currently doing.
    [<NoComparison>]
    type ProjectProgress =
        /// Started parsing a file (file path).
        | Starting of string

        /// Finished parsing a file (file path).
        | ReachedEnd of string * Suggestion.LintWarning list

        /// Failed to parse a file (file path, exception that caused failure).
        | Failed of string * System.Exception
    with
        /// Path of the F# file the progress information is for.
        member this.FilePath() =
            match this with
            | Starting(filePath)
            | ReachedEnd(filePath, _)
            | Failed(filePath, _) -> filePath

    [<NoEquality; NoComparison>]
    type LintInfo =
        { CancellationToken:CancellationToken option
          ErrorReceived:Suggestion.LintWarning -> unit
          ReportLinterProgress:ProjectProgress -> unit
          Configuration:Configuration.Configuration }

    type Context =
        { IndentationRuleContext:Map<int,bool*int>
          NoTabCharactersRuleContext:(string * Range) list }

    type RunAstNodeRulesConfig =
        {
            Rules: RuleMetadata<AstNodeRuleConfig>[]
            GlobalConfig: Rules.GlobalRuleConfig
            TypeCheckResults: FSharpCheckFileResults option
            ProjectCheckResults: FSharpCheckProjectResults option
            FilePath: string
            FileContent: string
            Lines: string[]
            SyntaxArray: AbstractSyntaxArray.Node array
        }

    let runAstNodeRules (config: RunAstNodeRulesConfig) =
        let mutable indentationRuleState = Map.empty
        let mutable noTabCharactersRuleState = List.empty

        let collect index (astNode: AbstractSyntaxArray.Node) =
            let getParents (depth:int) = AbstractSyntaxArray.getBreadcrumbs depth config.SyntaxArray index
            let astNodeParams =
                { 
                    AstNode = astNode.Actual
                    NodeHashcode = astNode.Hashcode
                    NodeIndex =  index
                    SyntaxArray = config.SyntaxArray
                    GetParents = getParents
                    FilePath = config.FilePath
                    FileContent = config.FileContent
                    Lines = config.Lines
                    CheckInfo = config.TypeCheckResults
                    ProjectCheckInfo = config.ProjectCheckResults
                    GlobalConfig = config.GlobalConfig
                }
            // Build state for rules with context.
            indentationRuleState <- Indentation.ContextBuilder.builder indentationRuleState astNode.Actual
            noTabCharactersRuleState <- NoTabCharacters.ContextBuilder.builder noTabCharactersRuleState astNode.Actual

            Array.collect
                (fun (rule: RuleMetadata<AstNodeRuleConfig>) -> runAstNodeRule rule astNodeParams)
                config.Rules

        // Collect suggestions for AstNode rules, and build context for following rules.
        let astNodeSuggestions =
            config.SyntaxArray
            |> Array.mapi (fun index astNode -> (index, astNode))
            |> Array.collect (fun (index, astNode) -> collect index astNode)

        let context =
            { IndentationRuleContext = indentationRuleState
              NoTabCharactersRuleContext = noTabCharactersRuleState }

        Array.iter (fun (rule: RuleMetadata<AstNodeRuleConfig>) -> rule.RuleConfig.Cleanup()) config.Rules
        (astNodeSuggestions, context)

    type RunLineRulesConfig =
        {
            LineRules: Configuration.LineRules
            GlobalConfig: Rules.GlobalRuleConfig
            FilePath: string
            FileContent: string
            Lines: string[]
            Context: Context
        }
    let runLineRules (config: RunLineRulesConfig) =
        let collectErrors (line: string) (lineNumber: int) (isLastLine: bool) = 
            let lineParams =
                {
                    LineRuleParams.Line = line
                    LineNumber = lineNumber + 1
                    IsLastLine = isLastLine
                    FilePath = config.FilePath
                    FileContent = config.FileContent
                    Lines = config.Lines
                    GlobalConfig = config.GlobalConfig
                }

            let indentationError =
                Option.map
                    (fun rule -> runLineRuleWithContext rule config.Context.IndentationRuleContext lineParams)
                    config.LineRules.IndentationRule

            let noTabCharactersError =
                Option.map
                    (fun rule -> runLineRuleWithContext rule config.Context.NoTabCharactersRuleContext lineParams)
                    config.LineRules.NoTabCharactersRule

            let lineErrors =
                Array.collect (fun rule -> runLineRule rule lineParams) config.LineRules.GenericLineRules

            [|
                Option.toArray indentationError
                Option.toArray noTabCharactersError
                Array.singleton lineErrors
            |]

        config.FileContent
        |> String.toLines
        |> Array.collect (fun (line, lineNumber, isLastLine) -> collectErrors line lineNumber isLastLine)
        |> Array.concat
        |> Array.concat

    let lint lintInfo (fileInfo:ParseFile.FileParseInfo) =
        let suggestionsRequiringTypeChecks = ConcurrentStack<_>()

        let fileWarnings = ResizeArray()

        let suggest (warning:Suggestion.LintWarning) =
            lintInfo.ErrorReceived warning
            fileWarnings.Add warning

        let trySuggest (suggestion:Suggestion.LintWarning) =
            if suggestion.Details.TypeChecks.IsEmpty then suggest suggestion
            else suggestionsRequiringTypeChecks.Push suggestion

        Starting(fileInfo.File) |> lintInfo.ReportLinterProgress

        let cancelHasNotBeenRequested () =
            match lintInfo.CancellationToken with
            | Some(value) -> not value.IsCancellationRequested
            | None -> true

        let enabledRules = Configuration.flattenConfig lintInfo.Configuration

        let lines = String.toLines fileInfo.Text |> Array.map (fun (line, _, _) -> line)
        let allRuleNames =
            [|
                enabledRules.LineRules.IndentationRule |> Option.map (fun rule -> rule.Name) |> Option.toArray
                enabledRules.LineRules.NoTabCharactersRule |> Option.map (fun rule -> rule.Name) |> Option.toArray
                Array.map (fun rule -> rule.Name) enabledRules.LineRules.GenericLineRules
                Array.map (fun rule -> rule.Name) enabledRules.AstNodeRules
            |] |> Array.concat |> Set.ofArray

        let suppressionInfo = Suppression.parseSuppressionInfo allRuleNames (Array.toList lines)

        try
            let syntaxArray = AbstractSyntaxArray.astToArray fileInfo.Ast

            // Collect suggestions for AstNode rules
            let (astNodeSuggestions, context) =
                runAstNodeRules
                    {
                        Rules = enabledRules.AstNodeRules
                        GlobalConfig = enabledRules.GlobalConfig
                        TypeCheckResults = fileInfo.TypeCheckResults
                        ProjectCheckResults = fileInfo.ProjectCheckResults
                        FilePath = fileInfo.File
                        FileContent = fileInfo.Text
                        Lines = lines
                        SyntaxArray = syntaxArray
                    }

            let lineSuggestions =
                runLineRules
                    {
                        LineRules = enabledRules.LineRules
                        GlobalConfig = enabledRules.GlobalConfig
                        FilePath = fileInfo.File
                        FileContent = fileInfo.Text
                        Lines = lines
                        Context = context
                    }

            [| lineSuggestions; astNodeSuggestions |]
            |> Array.concat
            |> Array.filter (fun warning ->
                let line = warning.Details.Range.StartLine
                Suppression.isSuppressed warning.RuleName line suppressionInfo |> not)
            |> Array.iter trySuggest

            if cancelHasNotBeenRequested () then
                try
                    let typeChecksSuccessful (typeChecks:(unit -> bool) list) =
                        (true, typeChecks)
                        ||> List.fold (fun acc cur -> acc && cur())

                    let typeCheckSuggestion (suggestion:Suggestion.LintWarning) =
                        if typeChecksSuccessful suggestion.Details.TypeChecks
                        then Some suggestion
                        else None

                    suggestionsRequiringTypeChecks
                    |> Seq.choose typeCheckSuggestion
                    |> Seq.iter suggest
                with
                | :? TimeoutException -> () // Do nothing.
        with
        | exn -> Failed(fileInfo.File, exn) |> lintInfo.ReportLinterProgress

        ReachedEnd(fileInfo.File, Seq.toList fileWarnings) |> lintInfo.ReportLinterProgress

    let getProjectInfo (projectFilePath:string) (toolsPath:Ionide.ProjInfo.Types.ToolsPath) =
        let errorMessageFromNotifications notifications =
            let extractError = function
            | Ionide.ProjInfo.Types.WorkspaceProjectState.Failed(_projFile, error) -> Some(string(error))
            | _ -> None

            notifications 
            |> Seq.tryPick extractError
            |> function Some(error) -> error | None -> "Unknown error when loading project file."

        let loader = Ionide.ProjInfo.WorkspaceLoader.Create toolsPath
        let notifications = ResizeArray<_>()
        loader.Notifications.Add notifications.Add
        let options = loader.LoadProjects [projectFilePath]
        options
        |> Seq.tryFind (fun opt -> opt.ProjectFileName = projectFilePath)
        |> Option.map (fun proj -> Ionide.ProjInfo.FCS.mapToFSharpProjectOptions proj options)
        |> function
            | Some proj -> Ok proj
            | None      -> errorMessageFromNotifications notifications |> Error

    let getFailedFiles = function
        | ParseFile.Failed failure -> Some failure
        | _ -> None

    let getParsedFiles = function
        | ParseFile.Success file -> Some file
        | _ -> None

    /// Result of running the linter.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type LintResult =
        | Success of Suggestion.LintWarning list
        | Failure of LintFailure

        member this.TryGetSuccess([<Out>] success:byref<Suggestion.LintWarning list>) =
            match this with
            | Success value -> success <- value; true
            | _ -> false
        member this.TryGetFailure([<Out>] failure:byref<LintFailure>) =
            match this with
            | Failure value -> failure <- value; true
            | _ -> false

    type ConfigurationParam =
        | Configuration of Configuration
        | FromFile of configPath:string
        /// Tries to load the config from file `fsharplint.json`.
        /// If this file doesn't exist or is invalid, falls back to the default configuration.
        | Default

    /// Optional parameters that can be provided to the linter.
    [<NoEquality; NoComparison>]
    type OptionalLintParameters = {
        /// Cancels a lint in progress.
        CancellationToken:CancellationToken option
        /// Lint configuration to use.
        /// Can either specify a full configuration object, or a path to a file to load the configuration from.
        /// You can also explicitly specify the default configuration.
        Configuration:ConfigurationParam
        /// This function will be called every time the linter finds a broken rule.
        ReceivedWarning:(Suggestion.LintWarning -> unit) option
        /// This function will be called any time the linter progress changes for a project.
        ReportLinterProgress:(ProjectProgress -> unit) option
    } with
        static member Default = {
            OptionalLintParameters.CancellationToken = None
            Configuration = Default
            ReceivedWarning = None
            ReportLinterProgress = None
        }

    /// If your application has already parsed the F# source files using `FSharp.Compiler.Services`
    /// you want to lint then this can be used to provide the parsed information to prevent the
    /// linter from parsing the file again.
    [<NoEquality; NoComparison>]
    type ParsedFileInformation = {
        /// File represented as an AST.
        Ast:FSharp.Compiler.Syntax.ParsedInput
        /// Contents of the file.
        Source:string
        /// Optional results of inferring the types on the AST (allows for a more accurate lint).
        TypeCheckResults:FSharpCheckFileResults option
        /// Optional results of project-wide type info (allows for a more accurate lint).
        ProjectCheckResults:FSharpCheckProjectResults option
    }

    /// Gets a FSharpLint Configuration based on the provided ConfigurationParam.
    let private getConfig (configParam:ConfigurationParam) =
        match configParam with
        | Configuration config -> Ok config
        | FromFile filePath ->
            try
                Configuration.loadConfig filePath
                |> Ok
            with
            | ex -> Error (string ex)
        | Default ->
            try
                Configuration.loadConfig $"./{Configuration.SettingsFileName}"
                |> Ok
            with
            | :? System.IO.FileNotFoundException ->
                Ok Configuration.defaultConfiguration
            | ex -> Error (string ex)

    /// Lints an entire F# project by retrieving the files from a given
    /// path to the `.fsproj` file.
    let asyncLintProject (optionalParams:OptionalLintParameters) (projectFilePath:string) (toolsPath:Ionide.ProjInfo.Types.ToolsPath) = async {
        if IO.File.Exists projectFilePath then
            let projectFilePath = Path.GetFullPath projectFilePath
            let lintWarnings = LinkedList<Suggestion.LintWarning>()

            match getConfig optionalParams.Configuration with
            | Ok config ->
                let projectProgress = Option.defaultValue ignore optionalParams.ReportLinterProgress

                let warningReceived (warning:Suggestion.LintWarning) =
                    lintWarnings.AddLast warning |> ignore<LinkedListNode<Suggestion.LintWarning>>

                    Option.iter (fun func -> func warning) optionalParams.ReceivedWarning

                let checker = FSharpChecker.Create(keepAssemblyContents=true)

                let parseFilesInProject files projectOptions = async {
                    let lintInformation =
                        { Configuration = config
                          CancellationToken = optionalParams.CancellationToken
                          ErrorReceived = warningReceived
                          ReportLinterProgress = projectProgress }

                    let isIgnoredFile filePath =
                        config.ignoreFiles
                        |> Option.map (fun ignoreFiles ->
                            let parsedIgnoreFiles = ignoreFiles |> Array.map IgnoreFiles.parseIgnorePath |> Array.toList
                            Configuration.IgnoreFiles.shouldFileBeIgnored parsedIgnoreFiles filePath)
                        |> Option.defaultValue false

                    let! parsedFiles =
                        files
                        |> List.filter (not << isIgnoredFile)
                        |> List.map (fun file -> ParseFile.parseFile file checker (Some projectOptions))
                        |> Async.Sequential

                    let failedFiles = Array.choose getFailedFiles parsedFiles

                    if Array.isEmpty failedFiles then
                        let! projectCheckResults = checker.ParseAndCheckProject projectOptions

                        parsedFiles
                        |> Array.choose getParsedFiles
                        |> Array.iter (fun fileParseResult -> 
                            lint 
                                lintInformation
                                { fileParseResult with ProjectCheckResults = Some projectCheckResults })

                        return Success ()
                    else
                        return Failure (FailedToParseFilesInProject (Array.toList failedFiles))
                }

                match getProjectInfo projectFilePath toolsPath with
                | Ok projectOptions ->
                    match! parseFilesInProject (Array.toList projectOptions.SourceFiles) projectOptions with
                    | Success _ -> return lintWarnings |> Seq.toList |> LintResult.Success
                    | Failure lintFailure -> return LintResult.Failure lintFailure
                | Error error ->
                    return 
                        MSBuildFailedToLoadProjectFile (projectFilePath, BuildFailure.InvalidProjectFileMessage error)
                        |> LintResult.Failure
            | Error err ->
                return RunTimeConfigError err |> LintResult.Failure
        else
            return FailedToLoadFile projectFilePath |> LintResult.Failure
    }

    [<Obsolete "Use asyncLintProject instead, otherwise this synchronous version might cause thread blocking issues; this API will be removed in the future.">]
    let lintProject optionalParams projectFilePath toolsPath =
        asyncLintProject optionalParams projectFilePath toolsPath |> Async.RunSynchronously

    /// Lints an entire F# solution by linting all projects specified in the `.sln`, `slnx` or `.slnf` file.
    let asyncLintSolution (optionalParams:OptionalLintParameters) (solutionFilePath:string) (toolsPath:Ionide.ProjInfo.Types.ToolsPath) = async {
        if IO.File.Exists solutionFilePath then
            let solutionFilePath = Path.GetFullPath solutionFilePath
            let solutionFolder = Path.GetDirectoryName solutionFilePath

            // Pre-load configuration so it isn't reloaded for every project.
            match getConfig optionalParams.Configuration with
            | Ok config ->
                let optionalParams = { optionalParams with Configuration = ConfigurationParam.Configuration config }

                try
                    // Use Microsoft.Build.Construction.SolutionFile for modern solution parsing
                    let solutionFile = SolutionFile.Parse(solutionFilePath)
                    
                    let projectsInSolution =
                        solutionFile.ProjectsInOrder
                        |> Seq.filter (fun project -> 
                            project.ProjectType = SolutionProjectType.KnownToBeMSBuildFormat && 
                            project.RelativePath.EndsWith(".fsproj"))
                        |> Seq.map (fun project -> 
                            let projectPath = Path.Combine(solutionFolder, project.RelativePath)
                            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                                projectPath
                            else // For non-Windows, ensure Unix format
                                projectPath.Replace("\\", "/"))
                        |> Seq.toArray

                    let! lintResults = 
                        projectsInSolution
                        |> Array.map (fun projectFilePath -> asyncLintProject optionalParams projectFilePath toolsPath)
                        |> Async.Sequential

                    let (successes, failures) =
                        lintResults
                        |> Array.fold (fun (successes, failures) result ->
                            match result with
                            | LintResult.Success warnings ->
                                (List.append warnings successes, failures)
                            | LintResult.Failure err ->
                                (successes, err :: failures)) (List.Empty, List.Empty)

                    match failures with
                    | [] ->
                        return LintResult.Success successes
                    | firstErr :: _ ->
                        return LintResult.Failure firstErr
                with
                | ex ->
                    return LintResult.Failure (MSBuildFailedToLoadProjectFile (solutionFilePath, BuildFailure.InvalidProjectFileMessage ex.Message))

            | Error err -> return LintResult.Failure (RunTimeConfigError err)
        else
            return FailedToLoadFile solutionFilePath |> LintResult.Failure
    }

    [<Obsolete "Use asyncLintSolution instead, otherwise this synchronous version might cause thread blocking issues; this API will be removed in the future.">]
    let lintSolution optionalParams solutionFilePath toolsPath =
        asyncLintSolution optionalParams solutionFilePath toolsPath |> Async.RunSynchronously

    /// Lints F# source code that has already been parsed using `FSharp.Compiler.Services` in the calling application.
    let lintParsedSource optionalParams parsedFileInfo =
        match getConfig optionalParams.Configuration with
        | Ok config ->
            let lintWarnings = LinkedList<Suggestion.LintWarning>()

            let warningReceived (warning:Suggestion.LintWarning) =
                lintWarnings.AddLast warning |> ignore<LinkedListNode<Suggestion.LintWarning>>

                Option.iter (fun func -> func warning) optionalParams.ReceivedWarning
            let lintInformation =
                { Configuration = config
                  CancellationToken = optionalParams.CancellationToken
                  ErrorReceived = warningReceived
                  ReportLinterProgress = Option.defaultValue ignore optionalParams.ReportLinterProgress }

            let parsedFileInfo =
                { ParseFile.Text = parsedFileInfo.Source
                  ParseFile.Ast = parsedFileInfo.Ast
                  ParseFile.TypeCheckResults = parsedFileInfo.TypeCheckResults
                  ParseFile.ProjectCheckResults = parsedFileInfo.ProjectCheckResults
                  ParseFile.File = "<inline source>" }

            lint lintInformation parsedFileInfo

            lintWarnings |> Seq.toList |> LintResult.Success
        | Error err ->
            LintResult.Failure (RunTimeConfigError err)

    /// Lints F# source code.
    let asyncLintSource optionalParams source =
        async {
            let checker = FSharpChecker.Create(keepAssemblyContents=true)

            match! ParseFile.parseSource source checker with
            | ParseFile.Success(parseFileInformation) ->
                let parsedFileInfo =
                    { Source = parseFileInformation.Text
                      Ast = parseFileInformation.Ast
                      TypeCheckResults = parseFileInformation.TypeCheckResults
                      ProjectCheckResults = None }

                return lintParsedSource optionalParams parsedFileInfo
            | ParseFile.Failed failure -> return LintResult.Failure(FailedToParseFile failure)
        }

    [<Obsolete "Use asyncLintSource instead, otherwise this synchronous version might cause thread blocking issues; this API will be removed in the future.">]
    let lintSource optionalParams source =
        asyncLintSource optionalParams source |> Async.RunSynchronously
        
    /// Lints an F# file that has already been parsed using `FSharp.Compiler.Services` in the calling application.
    let lintParsedFile (optionalParams:OptionalLintParameters) (parsedFileInfo:ParsedFileInformation) (filePath:string) =
        match getConfig optionalParams.Configuration with
        | Ok config ->
            let lintWarnings = LinkedList<Suggestion.LintWarning>()

            let warningReceived (warning:Suggestion.LintWarning) =
                lintWarnings.AddLast warning |> ignore<LinkedListNode<Suggestion.LintWarning>>

                Option.iter (fun func -> func warning) optionalParams.ReceivedWarning

            let lintInformation =
                { Configuration = config
                  CancellationToken = optionalParams.CancellationToken
                  ErrorReceived = warningReceived
                  ReportLinterProgress = Option.defaultValue ignore optionalParams.ReportLinterProgress }

            let parsedFileInfo =
                { ParseFile.Text = parsedFileInfo.Source
                  ParseFile.Ast = parsedFileInfo.Ast
                  ParseFile.TypeCheckResults = parsedFileInfo.TypeCheckResults
                  ParseFile.ProjectCheckResults = parsedFileInfo.ProjectCheckResults
                  ParseFile.File = filePath }

            lint lintInformation parsedFileInfo

            lintWarnings |> Seq.toList |> LintResult.Success
        | Error err -> LintResult.Failure (RunTimeConfigError err)

    /// Lints an F# file from a given path to the `.fs` file.
    let asyncLintFile optionalParams filePath = async {
        if IO.File.Exists filePath then
            let checker = FSharpChecker.Create(keepAssemblyContents=true)

            match! ParseFile.parseFile filePath checker None with
            | ParseFile.Success astFileParseInfo ->
                let parsedFileInfo =
                    { Source = astFileParseInfo.Text
                      Ast = astFileParseInfo.Ast
                      TypeCheckResults = astFileParseInfo.TypeCheckResults
                      ProjectCheckResults = astFileParseInfo.ProjectCheckResults }

                return lintParsedFile optionalParams parsedFileInfo filePath
            | ParseFile.Failed failure -> return LintResult.Failure(FailedToParseFile failure)
        else
            return FailedToLoadFile filePath |> LintResult.Failure
    }

    [<Obsolete "Use asyncLintFile instead, otherwise this synchronous version might cause thread blocking issues; this API will be removed in the future.">]
    let lintFile optionalParams filePath =
        asyncLintFile optionalParams filePath |> Async.RunSynchronously

    /// Lints multiple F# files from given file paths.
    let asyncLintFiles optionalParams filePaths = async {
        let checker = FSharpChecker.Create(keepAssemblyContents=true)
        
        match getConfig optionalParams.Configuration with
        | Ok config ->
            let optionalParams = { optionalParams with Configuration = ConfigurationParam.Configuration config }
            
            let lintSingleFile filePath = async {
                if IO.File.Exists filePath then
                    match! ParseFile.parseFile filePath checker None with
                    | ParseFile.Success astFileParseInfo ->
                        let parsedFileInfo =
                            { Source = astFileParseInfo.Text
                              Ast = astFileParseInfo.Ast
                              TypeCheckResults = astFileParseInfo.TypeCheckResults 
                              ProjectCheckResults = astFileParseInfo.ProjectCheckResults }
                        return lintParsedFile optionalParams parsedFileInfo filePath
                    | ParseFile.Failed failure ->
                        return LintResult.Failure (FailedToParseFile failure)
                else
                    return LintResult.Failure (FailedToLoadFile filePath)
            }
            
            let! results = filePaths |> Seq.map lintSingleFile |> Async.Sequential
            
            let failures = 
                results 
                |> Seq.choose (function | LintResult.Failure failure -> Some failure | _ -> None)
                |> Seq.toList
            let warnings = 
                results 
                |> Seq.collect (function | LintResult.Success warning -> warning | _ -> List.empty)
                |> Seq.toList
            
            match failures with
            | firstFailure :: _ -> return LintResult.Failure firstFailure
            | [] -> return LintResult.Success warnings
        | Error err ->
            return LintResult.Failure (RunTimeConfigError err)
    }

    [<Obsolete "Use asyncLintFiles instead, otherwise this synchronous version might cause thread blocking issues; this API will be removed in the future.">]
    let lintFiles optionalParams filePaths =
        asyncLintFiles optionalParams filePaths |> Async.RunSynchronously
