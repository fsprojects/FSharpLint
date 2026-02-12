module FSharpLint.Console.Program

open Argu
open System
open System.IO
open System.Reflection
open FSharpLint.Framework
open FSharpLint.Application

/// Output format the linter will use.
type private OutputFormat =
    | Standard = 1
    | MSBuild = 2

/// File type the linter is running against.
type internal FileType =
    | Project = 1
    | Solution = 2
    | File = 3
    | Source = 4
    | Wildcard = 5

type ExitCode =
    | Failure = -1
    | Success = 0

// Allowing underscores in union case names for proper Argu command line option formatting.
// fsharplint:disable UnionCasesNames
type private ToolArgs =
    | [<AltCommandLine("-f")>] Format of formatArg: OutputFormat
    | [<CliPrefix(CliPrefix.None)>] Lint of lintArgs: ParseResults<LintArgs>
    | Version
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Format _ -> "Output format of the linter."
            | Lint _ -> "Runs FSharpLint against a file or a collection of files."
            | Version -> "Prints current version."

// TODO: investigate erroneous warning on this type definition
// fsharplint:disable UnionDefinitionIndentation
and private LintArgs =
    | [<MainCommand; Mandatory>] Target of target:string
    | [<AltCommandLine("-l")>] Lint_Config of lintConfig:string
    | File_Type of FileType
// fsharplint:enable UnionDefinitionIndentation
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "Input to lint."
            | File_Type _ -> "Input type the linter will run against. If this is not set, the file type will be inferred from the file extension."
            | Lint_Config _ -> "Path to the config for the lint."
// fsharplint:enable UnionCasesNames

/// Expands a wildcard pattern to a list of matching files.
/// Supports recursive search using ** (e.g., "**/*.fs" or "src/**/*.fs")
let internal expandWildcard (pattern:string) =
    let validTargetFileExtensions = [ ".fs"; ".fsx"; ".fsproj"; ".sln"; ".slnx" ]
    let isValidTargetFile (filePath:string) =
        validTargetFileExtensions |> List.exists filePath.EndsWith
    
    let normalizedPattern = pattern.Replace('\\', '/')
    
    let directory, searchPattern, searchOption =
        match normalizedPattern.IndexOf "**/" with
        | -1 ->
            // Non-recursive pattern
            match normalizedPattern.LastIndexOf '/' with
            | -1 -> (".", normalizedPattern, SearchOption.TopDirectoryOnly)
            | lastSeparator ->
                let dir = normalizedPattern.Substring(0, lastSeparator)
                let pat = normalizedPattern.Substring(lastSeparator + 1)
                ((if String.IsNullOrEmpty dir then "." else dir), pat, SearchOption.TopDirectoryOnly)
        | 0 ->
            // Pattern starts with **/
            let pat = normalizedPattern.Substring 3
            (".", pat, SearchOption.AllDirectories)
        | doubleStarIndex ->
            // Pattern has **/ in the middle
            let dir = normalizedPattern.Substring(0, doubleStarIndex).TrimEnd '/'
            let pat = normalizedPattern.Substring(doubleStarIndex + 3)
            (dir, pat, SearchOption.AllDirectories)
    
    let fullDirectory = Path.GetFullPath directory
    if Directory.Exists fullDirectory then
        Directory.GetFiles(fullDirectory, searchPattern, searchOption)
        |> Array.filter isValidTargetFile
        |> Array.toList
    else
        List.empty

/// Checks if a string contains wildcard characters.
let internal containsWildcard (target:string) =
    target.Contains("*") || target.Contains("?")

/// Infers the file type of the target based on its file extension.
let internal inferFileType (target:string) =
    if containsWildcard target then
        FileType.Wildcard
    else if target.EndsWith ".fs" || target.EndsWith ".fsx" then
        FileType.File
    else if target.EndsWith ".fsproj" then
        FileType.Project
    else if target.EndsWith ".slnx" || target.EndsWith ".slnf" || target.EndsWith ".sln" then
        FileType.Solution
    else
        FileType.Source

/// Must be called only once per process.
/// We're calling it globally so we can call main multiple times from our tests.
let globalToolsPath = Ionide.ProjInfo.Init.init (DirectoryInfo <| Directory.GetCurrentDirectory())  None

[<EntryPoint>]
let main argv =
    let parserProgress (output:Output.IOutput) = function
        | Starting file ->
            String.Format(Resources.GetString("ConsoleStartingFile"), file) |> output.WriteInfo
        | ReachedEnd (_, warnings) ->
            String.Format(Resources.GetString("ConsoleFinishedFile"), List.length warnings) |> output.WriteInfo
        | Failed (file, parseException) ->
            String.Format(Resources.GetString("ConsoleFailedToParseFile"), file) |> output.WriteError
            output.WriteError
                $"Exception Message:{Environment.NewLine}{parseException.Message}{Environment.NewLine}Exception Stack Trace:{Environment.NewLine}{parseException.StackTrace}{Environment.NewLine}"

    let lint
        (lintArgs: ParseResults<LintArgs>)
        (output: Output.IOutput)
        (toolsPath:Ionide.ProjInfo.Types.ToolsPath)
        : ExitCode =
        let mutable exitCode = ExitCode.Success

        let handleError (str:string) =
            output.WriteError str
            exitCode <- ExitCode.Failure

        let handleLintResult = function
            | LintResult.Success(warnings) ->
                String.Format(Resources.GetString("ConsoleFinished"), List.length warnings)
                |> output.WriteInfo
                if not (List.isEmpty warnings) then
                    exitCode <- ExitCode.Failure
            | LintResult.Failure(failure) ->
                handleError failure.Description

        let lintConfig = lintArgs.TryGetResult Lint_Config

        let configParam =
            match lintConfig with
            | Some configPath -> FromFile configPath
            | None -> Default

        let lintParams =
            {
                CancellationToken = None
                ReceivedWarning = Some output.WriteWarning
                Configuration = configParam
                ReportLinterProgress = Some (parserProgress output)
            }

        let target = lintArgs.GetResult Target
        let fileType = lintArgs.TryGetResult File_Type |> Option.defaultValue (inferFileType target)

        let warningPrefix = "WARNING:"
        let projectRecommendationMessage = "Using a project (slnx/sln/fsproj) can detect more issues."

        try
            match Lint.getConfig lintParams.Configuration with
            | Ok config ->
                let loadedRules = Configuration.flattenConfig config
                let enabledRulesCount = loadedRules.Count
                let allRulesCount = loadedRules.TotalRuleCount
                let disabledRulesCount = allRulesCount - enabledRulesCount
                output.WriteInfo $"Running FSharpLint with {allRulesCount} rules ({enabledRulesCount} enabled, {disabledRulesCount} disabled)..."
            | Error _ -> ()

            let lintResult =
                match fileType with
                | FileType.File -> 
                    if target.EndsWith ".fs" then
                        output.WriteError $"{warningPrefix} Going to analyze single .fs file, but not recommended. {projectRecommendationMessage}"
                    Lint.asyncLintFile lintParams target |> Async.RunSynchronously
                | FileType.Source -> Lint.asyncLintSource lintParams target |> Async.RunSynchronously
                | FileType.Solution -> Lint.asyncLintSolution lintParams target toolsPath |> Async.RunSynchronously
                | FileType.Wildcard ->
                    let files = expandWildcard target
                    if List.isEmpty files then
                        output.WriteInfo $"No files matching pattern '%s{target}' were found."
                        LintResult.Success List.empty
                    else
                        let fileTypes = files |> List.map inferFileType
                        if fileTypes |> List.forall (fun aType -> aType = FileType.File) then
                            output.WriteError $"{warningPrefix} Wildcard detected, but not recommended. {projectRecommendationMessage}"
                        output.WriteInfo $"Found %d{List.length files} file(s) matching pattern '%s{target}'."
                        let results = 
                            let getResult file inferredFileType =
                                match inferredFileType with
                                | FileType.File ->  Lint.asyncLintFile lintParams file
                                | FileType.Solution -> Lint.asyncLintSolution lintParams file toolsPath
                                | FileType.Project
                                | _ -> Lint.asyncLintProject lintParams file toolsPath

                            List.map2 getResult files fileTypes
                            |> List.map Async.RunSynchronously
                                
                        let failures = 
                            results 
                            |> List.choose (function | LintResult.Failure failure -> Some failure | _ -> None)
                        let warnings = 
                            results 
                            |> List.collect (function | LintResult.Success warning -> warning | _ -> List.empty)
            
                        match failures with
                        | firstFailure :: _ -> LintResult.Failure firstFailure
                        | [] -> LintResult.Success warnings
                | FileType.Project
                | _ -> Lint.asyncLintProject lintParams target toolsPath |> Async.RunSynchronously
            handleLintResult lintResult
        with
        | exn ->
            let targetStr = if fileType = FileType.Source then "source" else target
            handleError
                $"Lint failed while analysing %s{targetStr}.{Environment.NewLine}Failed with: %s{exn.Message}{Environment.NewLine}Stack trace: {exn.StackTrace}"

        exitCode

    let start (arguments:ParseResults<ToolArgs>) (toolsPath:Ionide.ProjInfo.Types.ToolsPath) =
        let output =
            match arguments.TryGetResult Format with
            | Some OutputFormat.MSBuild -> Output.MSBuildOutput() :> Output.IOutput
            | Some OutputFormat.Standard
            | Some _
            | None -> Output.StandardOutput() :> Output.IOutput

        if arguments.Contains ToolArgs.Version then
            let maybeVersion =
                Assembly.GetExecutingAssembly().GetCustomAttributes false
                |> Seq.tryPick (function | :? AssemblyInformationalVersionAttribute as aiva -> Some aiva.InformationalVersion | _ -> None)
            match maybeVersion with
            | Some version ->
                output.WriteInfo $"Current version: {version}"
                Environment.Exit 0
            | None ->
                failwith "Error: unable to get version"

        match arguments.GetSubCommand() with
        | Lint lintArgs ->
            lint lintArgs output toolsPath
        | _ ->
            ExitCode.Failure

    let errorHandler = ProcessExiter(colorizer = function
        | ErrorCode.HelpText -> None
        | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<ToolArgs>(programName = "fsharplint", errorHandler = errorHandler)
    let parseResults = parser.ParseCommandLine argv
    start parseResults globalToolsPath
    |> int
