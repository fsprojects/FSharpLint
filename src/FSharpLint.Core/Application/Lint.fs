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

        member this.Description
            with get() =
                let getParseFailureReason = function
                    | ParseFile.FailedToParseFile failures ->
                        let getFailureReason (x:FSharpDiagnostic) =
                            $"failed to parse file {x.FileName}, message: {x.Message}"

                        String.Join(", ", failures |> Array.map getFailureReason)
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

    [<NoComparison>]
    type Result<'T> =
        | Success of 'T
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
            | Starting(f)
            | ReachedEnd(f, _)
            | Failed(f, _) -> f

    [<NoEquality; NoComparison>]
    type LintInfo =
        { CancellationToken:CancellationToken option
          ErrorReceived:Suggestion.LintWarning -> unit
          ReportLinterProgress:ProjectProgress -> unit
          Configuration:Configuration.Configuration }

    type Context =
        { IndentationRuleContext:Map<int,bool*int>
          NoTabCharactersRuleContext:(string * Range) list }

    let runAstNodeRules (rules:RuleMetadata<AstNodeRuleConfig> []) (globalConfig:Rules.GlobalRuleConfig) typeCheckResults (filePath:string) (fileContent:string) (lines:string []) syntaxArray =
        let mutable indentationRuleState = Map.empty
        let mutable noTabCharactersRuleState = List.empty

        let collect i (astNode: AbstractSyntaxArray.Node) =
            let getParents (depth:int) = AbstractSyntaxArray.getBreadcrumbs depth syntaxArray i
            let astNodeParams =
                { 
                    AstNode = astNode.Actual
                    NodeHashcode = astNode.Hashcode
                    NodeIndex =  i
                    SyntaxArray = syntaxArray
                    GetParents = getParents
                    FilePath = filePath
                    FileContent = fileContent
                    Lines = lines
                    CheckInfo = typeCheckResults
                    GlobalConfig = globalConfig
                }
            // Build state for rules with context.
            indentationRuleState <- Indentation.ContextBuilder.builder indentationRuleState astNode.Actual
            noTabCharactersRuleState <- NoTabCharacters.ContextBuilder.builder noTabCharactersRuleState astNode.Actual

            rules
            |> Array.collect (fun rule -> runAstNodeRule rule astNodeParams)

        // Collect suggestions for AstNode rules, and build context for following rules.
        let astNodeSuggestions =
            syntaxArray
            |> Array.mapi (fun i astNode -> (i, astNode))
            |> Array.collect (fun (i, astNode) -> collect i astNode)

        let context =
            { IndentationRuleContext = indentationRuleState
              NoTabCharactersRuleContext = noTabCharactersRuleState }

        rules |> Array.iter (fun rule -> rule.RuleConfig.Cleanup())
        (astNodeSuggestions, context)

    let runLineRules (lineRules:Configuration.LineRules) (globalConfig:Rules.GlobalRuleConfig) (filePath:string) (fileContent:string) (lines:string []) (context:Context) =
        let collectErrors (line: string) (lineNumber: int) (isLastLine: bool) = 
            let lineParams =
                { 
                    LineRuleParams.Line = line
                    LineNumber = lineNumber + 1
                    IsLastLine = isLastLine
                    FilePath = filePath
                    FileContent = fileContent
                    Lines = lines
                    GlobalConfig = globalConfig
                }

            let indentationError =
                lineRules.IndentationRule
                |> Option.map (fun rule -> runLineRuleWithContext rule context.IndentationRuleContext lineParams)

            let noTabCharactersError =
                lineRules.NoTabCharactersRule
                |> Option.map (fun rule -> runLineRuleWithContext rule context.NoTabCharactersRuleContext lineParams)

            let lineErrors =
                lineRules.GenericLineRules
                |> Array.collect (fun rule -> runLineRule rule lineParams)

            [|
                indentationError |> Option.toArray
                noTabCharactersError |> Option.toArray
                lineErrors |> Array.singleton
            |]

        fileContent
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
            | Some(x) -> not x.IsCancellationRequested
            | None -> true

        let enabledRules = Configuration.flattenConfig lintInfo.Configuration

        let lines = String.toLines fileInfo.Text |> Array.map (fun (line, _, _) -> line)
        let allRuleNames =
            [|
                enabledRules.LineRules.IndentationRule |> Option.map (fun rule -> rule.Name) |> Option.toArray
                enabledRules.LineRules.NoTabCharactersRule |> Option.map (fun rule -> rule.Name) |> Option.toArray
                enabledRules.LineRules.GenericLineRules |> Array.map (fun rule -> rule.Name)
                enabledRules.AstNodeRules |> Array.map (fun rule -> rule.Name)
            |] |> Array.concat |> Set.ofArray

        let suppressionInfo = Suppression.parseSuppressionInfo allRuleNames (Array.toList lines)

        try
            let syntaxArray = AbstractSyntaxArray.astToArray fileInfo.Ast

            // Collect suggestions for AstNode rules
            let (astNodeSuggestions, context) = runAstNodeRules enabledRules.AstNodeRules enabledRules.GlobalConfig fileInfo.TypeCheckResults fileInfo.File fileInfo.Text lines syntaxArray
            let lineSuggestions = runLineRules enabledRules.LineRules enabledRules.GlobalConfig fileInfo.File fileInfo.Text lines context

            [| lineSuggestions; astNodeSuggestions |]
            |> Array.concat
            |> Array.filter (fun warning ->
                let line = warning.Details.Range.StartLine
                Suppression.isSuppressed warning.RuleName line suppressionInfo |> not)
            |> Array.iter trySuggest

            if cancelHasNotBeenRequested () then
                let runSynchronously work =
                    let timeoutMs = 2000
                    match lintInfo.CancellationToken with
                    | Some(cancellationToken) -> Async.RunSynchronously(work, timeoutMs, cancellationToken)
                    | None -> Async.RunSynchronously(work, timeoutMs)

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
        | e -> Failed(fileInfo.File, e) |> lintInfo.ReportLinterProgress

        ReachedEnd(fileInfo.File, fileWarnings |> Seq.toList) |> lintInfo.ReportLinterProgress

    let private runProcess (workingDir:string) (exePath:string) (args:string) =
        let psi = 
            System.Diagnostics.ProcessStartInfo(
                FileName = exePath,
                WorkingDirectory = workingDir,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                Arguments = args,
                CreateNoWindow = true,
                UseShellExecute = false
            )

        use p = new System.Diagnostics.Process(StartInfo = psi)
        let sbOut = System.Text.StringBuilder()
        p.OutputDataReceived.Add(fun ea -> sbOut.AppendLine(ea.Data) |> ignore<Text.StringBuilder>)
        let sbErr = System.Text.StringBuilder()
        p.ErrorDataReceived.Add(fun ea -> sbErr.AppendLine(ea.Data) |> ignore<Text.StringBuilder>)
        p.Start() |> ignore<bool>
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()

        let exitCode = p.ExitCode

        (exitCode, (workingDir, exePath, args))

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

        member self.TryGetSuccess([<Out>] success:byref<Suggestion.LintWarning list>) =
            match self with
            | Success value -> success <- value; true
            | _ -> false
        member self.TryGetFailure([<Out>] failure:byref<LintFailure>) =
            match self with
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
                Configuration.loadConfig "./fsharplint.json"
                |> Ok
            with
            | :? System.IO.FileNotFoundException ->
                Ok Configuration.defaultConfiguration
            | ex -> Error (string ex)

    /// Lints an entire F# project by retrieving the files from a given
    /// path to the `.fsproj` file.
    let lintProject (optionalParams:OptionalLintParameters) (projectFilePath:string) (toolsPath:Ionide.ProjInfo.Types.ToolsPath) =
        if IO.File.Exists projectFilePath then
            let projectFilePath = Path.GetFullPath projectFilePath
            let lintWarnings = LinkedList<Suggestion.LintWarning>()

            match getConfig optionalParams.Configuration with
            | Ok config ->
                let projectProgress = Option.defaultValue ignore optionalParams.ReportLinterProgress

                let warningReceived (warning:Suggestion.LintWarning) =
                    lintWarnings.AddLast warning |> ignore<LinkedListNode<Suggestion.LintWarning>>

                    optionalParams.ReceivedWarning |> Option.iter (fun func -> func warning)

                let checker = FSharpChecker.Create(keepAssemblyContents=true)

                let parseFilesInProject files projectOptions =
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

                    let parsedFiles =
                        files
                        |> List.filter (not << isIgnoredFile)
                        |> List.map (fun file -> ParseFile.parseFile file checker (Some projectOptions))

                    let failedFiles = parsedFiles |> List.choose getFailedFiles

                    if List.isEmpty failedFiles then
                        parsedFiles
                        |> List.choose getParsedFiles
                        |> List.iter (lint lintInformation)

                        Success ()
                    else
                        Failure (FailedToParseFilesInProject failedFiles)

                match getProjectInfo projectFilePath toolsPath with
                | Ok projectOptions ->
                    match parseFilesInProject (Array.toList projectOptions.SourceFiles) projectOptions with
                    | Success _ -> lintWarnings |> Seq.toList |> LintResult.Success
                    | Failure x -> LintResult.Failure x
                | Error error ->
                    MSBuildFailedToLoadProjectFile (projectFilePath, BuildFailure.InvalidProjectFileMessage error)
                    |> LintResult.Failure
            | Error err ->
                RunTimeConfigError err
                |> LintResult.Failure
        else
            FailedToLoadFile projectFilePath
            |> LintResult.Failure

    /// Lints an entire F# solution by linting all projects specified in the `.sln`, `slnx` or `.slnf` file.
    let lintSolution (optionalParams:OptionalLintParameters) (solutionFilePath:string) (toolsPath:Ionide.ProjInfo.Types.ToolsPath) =
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

                    let (successes, failures) =
                        projectsInSolution
                        |> Array.map (fun projectFilePath -> lintProject optionalParams projectFilePath toolsPath)
                        |> Array.fold (fun (successes, failures) result ->
                            match result with
                            | LintResult.Success warnings ->
                                (List.append warnings successes, failures)
                            | LintResult.Failure err ->
                                (successes, err :: failures)) (List.Empty, List.Empty)

                    match failures with
                    | [] ->
                        LintResult.Success successes
                    | firstErr :: _ ->
                        LintResult.Failure firstErr
                with
                | ex ->
                    LintResult.Failure (MSBuildFailedToLoadProjectFile (solutionFilePath, BuildFailure.InvalidProjectFileMessage ex.Message))

            | Error err -> LintResult.Failure (RunTimeConfigError err)
        else
            FailedToLoadFile solutionFilePath
            |> LintResult.Failure

    /// Lints F# source code that has already been parsed using `FSharp.Compiler.Services` in the calling application.
    let lintParsedSource optionalParams parsedFileInfo =
        match getConfig optionalParams.Configuration with
        | Ok config ->
            let lintWarnings = LinkedList<Suggestion.LintWarning>()

            let warningReceived (warning:Suggestion.LintWarning) =
                lintWarnings.AddLast warning |> ignore<LinkedListNode<Suggestion.LintWarning>>

                optionalParams.ReceivedWarning |> Option.iter (fun func -> func warning)
            let lintInformation =
                { Configuration = config
                  CancellationToken = optionalParams.CancellationToken
                  ErrorReceived = warningReceived
                  ReportLinterProgress = Option.defaultValue ignore optionalParams.ReportLinterProgress }

            let parsedFileInfo =
                { ParseFile.Text = parsedFileInfo.Source
                  ParseFile.Ast = parsedFileInfo.Ast
                  ParseFile.TypeCheckResults = parsedFileInfo.TypeCheckResults
                  ParseFile.File = "<inline source>" }

            lint lintInformation parsedFileInfo

            lintWarnings |> Seq.toList |> LintResult.Success
        | Error err ->
            LintResult.Failure (RunTimeConfigError err)

    /// Lints F# source code.
    let lintSource optionalParams source =
        let checker = FSharpChecker.Create(keepAssemblyContents=true)

        match ParseFile.parseSource source checker with
        | ParseFile.Success(parseFileInformation) ->
            let parsedFileInfo =
                { Source = parseFileInformation.Text
                  Ast = parseFileInformation.Ast
                  TypeCheckResults = parseFileInformation.TypeCheckResults }

            lintParsedSource optionalParams parsedFileInfo
        | ParseFile.Failed failure -> LintResult.Failure(FailedToParseFile failure)

    /// Lints an F# file that has already been parsed using `FSharp.Compiler.Services` in the calling application.
    let lintParsedFile (optionalParams:OptionalLintParameters) (parsedFileInfo:ParsedFileInformation) (filePath:string) =
        match getConfig optionalParams.Configuration with
        | Ok config ->
            let lintWarnings = LinkedList<Suggestion.LintWarning>()

            let warningReceived (warning:Suggestion.LintWarning) =
                lintWarnings.AddLast warning |> ignore<LinkedListNode<Suggestion.LintWarning>>

                optionalParams.ReceivedWarning |> Option.iter (fun func -> func warning)

            let lintInformation =
                { Configuration = config
                  CancellationToken = optionalParams.CancellationToken
                  ErrorReceived = warningReceived
                  ReportLinterProgress = Option.defaultValue ignore optionalParams.ReportLinterProgress }

            let parsedFileInfo =
                { ParseFile.Text = parsedFileInfo.Source
                  ParseFile.Ast = parsedFileInfo.Ast
                  ParseFile.TypeCheckResults = parsedFileInfo.TypeCheckResults
                  ParseFile.File = filePath }

            lint lintInformation parsedFileInfo

            lintWarnings |> Seq.toList |> LintResult.Success
        | Error err -> LintResult.Failure (RunTimeConfigError err)

    /// Lints an F# file from a given path to the `.fs` file.
    let lintFile optionalParams filePath =
        if IO.File.Exists filePath then
            let checker = FSharpChecker.Create(keepAssemblyContents=true)

            match ParseFile.parseFile filePath checker None with
            | ParseFile.Success astFileParseInfo ->
                let parsedFileInfo =
                    { Source = astFileParseInfo.Text
                      Ast = astFileParseInfo.Ast
                      TypeCheckResults = astFileParseInfo.TypeCheckResults }

                lintParsedFile optionalParams parsedFileInfo filePath
            | ParseFile.Failed failure -> LintResult.Failure(FailedToParseFile failure)
        else
            FailedToLoadFile filePath
            |> LintResult.Failure
