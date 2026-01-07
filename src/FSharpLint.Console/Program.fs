module FSharpLint.Console.Program

open Argu
open System
open System.IO
open System.Reflection
open System.Linq
open System.Text
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
    | NoSuchRuleName = 1
    | NoSuggestedFix = 2

let fileTypeHelp = "Input type the linter will run against. If this is not set, the file type will be inferred from the file extension."

// Allowing underscores in union case names for proper Argu command line option formatting.
// fsharplint:disable UnionCasesNames
type private ToolArgs =
    | [<AltCommandLine("-f")>] Format of OutputFormat
    | [<CliPrefix(CliPrefix.None)>] Lint of ParseResults<LintArgs>
    | [<CliPrefix(CliPrefix.None)>] Fix of ParseResults<FixArgs>
    | Version
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Format _ -> "Output format of the linter."
            | Lint _ -> "Runs FSharpLint against a file or a collection of files."
            | Fix _ -> "Apply quickfixes for specified rule name or names (comma separated)."
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
            | File_Type _ -> fileTypeHelp
            | Lint_Config _ -> "Path to the config for the lint."

// TODO: investigate erroneous warning on this type definition
// fsharplint:disable UnionDefinitionIndentation
and private FixArgs =
    | [<MainCommand; Mandatory>] Fix_Target of ruleName:string * target:string
    | Fix_File_Type of FileType
// fsharplint:enable UnionDefinitionIndentation
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Fix_Target _ -> "Rule name to be applied with suggestedFix and input to lint."
            | Fix_File_Type _ -> fileTypeHelp
// fsharplint:enable UnionCasesNames

type private LintingArgs =
    {
        FileType: FileType
        LintParams: OptionalLintParameters
        Target: string
        ToolsPath: Ionide.ProjInfo.Types.ToolsPath
        RuleNameToApplySuggestedFixFrom: string option
    }

/// Expands a wildcard pattern to a list of matching files.
/// Supports recursive search using ** (e.g., "**/*.fs" or "src/**/*.fs")
let internal expandWildcard (pattern:string) =
    let isFSharpFile (filePath:string) =
        filePath.EndsWith ".fs" || filePath.EndsWith ".fsx"
    
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
        |> Array.filter isFSharpFile
        |> Array.toList
    else
        List.empty

let private parserProgress (output:Output.IOutput) = function
    | Starting file ->
        String.Format(Resources.GetString("ConsoleStartingFile"), file) |> output.WriteInfo
    | ReachedEnd (_, warnings) ->
        String.Format(Resources.GetString("ConsoleFinishedFile"), List.length warnings) |> output.WriteInfo
    | Failed (file, parseException) ->
        String.Format(Resources.GetString("ConsoleFailedToParseFile"), file) |> output.WriteError
        output.WriteError
            $"Exception Message:{Environment.NewLine}{parseException.Message}{Environment.NewLine}Exception Stack Trace:{Environment.NewLine}{parseException.StackTrace}{Environment.NewLine}"

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

let private outputWarnings (output: Output.IOutput) (warnings: List<Suggestion.LintWarning>) =
    String.Format(Resources.GetString "ConsoleFinished", List.length warnings)
        |> output.WriteInfo
    
let private handleLintResult (output: Output.IOutput) (lintResult: LintResult) =
    match lintResult with
    | LintResult.Success warnings ->
        outputWarnings output warnings
        if List.isEmpty warnings |> not then 
            ExitCode.Failure
        else
            ExitCode.Success
    | LintResult.Failure failure -> 
        output.WriteError failure.Description
        ExitCode.Failure 
   
let private getParams (output: Output.IOutput) config =
    let paramConfig =
        match config with
        | Some configPath -> FromFile configPath
        | None -> Default

    { CancellationToken = None
      ReceivedWarning = Some output.WriteWarning
      Configuration = paramConfig
      ReportLinterProgress = parserProgress output |> Some }

let private handleFixResult (output: Output.IOutput) (target: string) (ruleName: string) (lintResult: LintResult) : ExitCode =
    match lintResult with
    | LintResult.Success warnings ->
        String.Format(Resources.GetString "ConsoleApplyingSuggestedFixFile", target) |> output.WriteInfo
        let increment = 1
        let noFixIncrement = 0

        let countFixes (element: Suggestion.LintWarning) =
            let sourceCode = File.ReadAllText element.FilePath
            if String.Equals(ruleName, element.RuleName, StringComparison.InvariantCultureIgnoreCase) then
                match element.Details.SuggestedFix with
                | Some lazySuggestedFix ->
                    match lazySuggestedFix.Force() with
                    | Some suggestedFix ->
                        let updatedSourceCode = 
                            let builder = StringBuilder(sourceCode.Length + suggestedFix.ToText.Length)
                            let firstPart = 
                                sourceCode.AsSpan(
                                    0,
                                    (ExpressionUtilities.findPos suggestedFix.FromRange.Start sourceCode)
                                    |> Option.defaultWith 
                                        (fun () -> failwith "Could not find index from position (suggestedFix.FromRange.Start)")
                                )
                            let secondPart = 
                                sourceCode.AsSpan
                                    (ExpressionUtilities.findPos suggestedFix.FromRange.End sourceCode
                                     |> Option.defaultWith 
                                         (fun () -> failwith "Could not find index from position (suggestedFix.FromRange.End)"))
                            builder
                                .Append(firstPart)
                                .Append(suggestedFix.ToText)
                                .Append(secondPart)
                                .ToString()
                        File.WriteAllText(
                            element.FilePath,
                            updatedSourceCode,
                            Encoding.UTF8)
                    | _ -> ()
                    increment
                | None -> noFixIncrement
            else
                noFixIncrement

        let countSuggestedFix = 
            warnings |> List.sumBy countFixes
        outputWarnings output warnings

        if countSuggestedFix > 0 then
            ExitCode.Success
        else
            ExitCode.NoSuggestedFix

    | LintResult.Failure failure ->
        output.WriteError failure.Description
        ExitCode.Failure

let private performLinting (output: Output.IOutput) (args: LintingArgs) =
    try
        let lintResult =
            match args.FileType with
            | FileType.File -> Lint.asyncLintFile args.LintParams args.Target |> Async.RunSynchronously
            | FileType.Source -> Lint.asyncLintSource args.LintParams args.Target |> Async.RunSynchronously
            | FileType.Solution -> Lint.asyncLintSolution args.LintParams args.Target args.ToolsPath |> Async.RunSynchronously
            | FileType.Wildcard ->
                output.WriteInfo "Wildcard detected, but not recommended. Using a project (slnx/sln/fsproj) can detect more issues."
                let files = expandWildcard args.Target
                if List.isEmpty files then
                    output.WriteInfo $"No files matching pattern '%s{args.Target}' were found."
                    LintResult.Success List.empty
                else
                    output.WriteInfo $"Found %d{List.length files} file(s) matching pattern '%s{args.Target}'."
                    Lint.asyncLintFiles args.LintParams files |> Async.RunSynchronously
            | FileType.Project
            | _ -> Lint.asyncLintProject args.LintParams args.Target args.ToolsPath |> Async.RunSynchronously
        
        match args.RuleNameToApplySuggestedFixFrom with
        | Some ruleName -> handleFixResult output args.Target ruleName lintResult
        | None -> handleLintResult output lintResult
    with
    | exn ->
        let target = if args.FileType = FileType.Source then "source" else args.Target
        output.WriteError $"Lint failed while analysing %s{target}.{Environment.NewLine}Failed with: %s{exn.Message}{Environment.NewLine}Stack trace: {exn.StackTrace}"
        ExitCode.Failure

let private lint
    (lintArgs: ParseResults<LintArgs>)
    (output: Output.IOutput)
    (toolsPath:Ionide.ProjInfo.Types.ToolsPath)
    : ExitCode =
    let lintConfig = lintArgs.TryGetResult Lint_Config

    let lintParams = getParams output lintConfig
    let target = lintArgs.GetResult Target
    let fileType = lintArgs.TryGetResult File_Type |> Option.defaultValue (inferFileType target)

    performLinting
        output
        { FileType = fileType
          LintParams = lintParams
          Target = target
          ToolsPath = toolsPath
          RuleNameToApplySuggestedFixFrom = None }

let private applySuggestedFix (fixArgs: ParseResults<FixArgs>) (output: Output.IOutput) toolsPath =
    let fixParams = getParams output None
    let ruleName, target = fixArgs.GetResult Fix_Target
    let fileType = fixArgs.TryGetResult Fix_File_Type |> Option.defaultValue (inferFileType target)
        
    let allRules = 
        match getConfig fixParams.Configuration with
        | Ok config -> Some (Configuration.flattenConfig config false)
        | _ -> None

    let allRuleNames =
        match allRules with
        | Some rules ->  (fun (loadedRules:Configuration.LoadedRules) -> ([|
            loadedRules.LineRules.IndentationRule |> Option.map (fun rule -> rule.Name) |> Option.toArray
            loadedRules.LineRules.NoTabCharactersRule |> Option.map (fun rule -> rule.Name) |> Option.toArray
            loadedRules.LineRules.GenericLineRules |> Array.map (fun rule -> rule.Name)
            loadedRules.AstNodeRules |> Array.map (fun rule -> rule.Name)
            |] |> Array.concat |> Set.ofArray)) rules
        | _ -> Set.empty

    if allRuleNames.Any(fun aRuleName -> String.Equals(aRuleName, ruleName, StringComparison.InvariantCultureIgnoreCase)) then
        performLinting
            output
            { FileType = fileType
              LintParams = fixParams
              Target = target
              ToolsPath = toolsPath
              RuleNameToApplySuggestedFixFrom = Some ruleName }
    else
        output.WriteError <| sprintf "Rule '%s' does not exist." ruleName 
        ExitCode.NoSuchRuleName

let private start (arguments:ParseResults<ToolArgs>) (toolsPath:Ionide.ProjInfo.Types.ToolsPath) =
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
    | Fix fixArgs ->
        applySuggestedFix fixArgs output toolsPath
    | _ ->
        ExitCode.Failure

/// Must be called only once per process.
/// We're calling it globally so we can call main multiple times from our tests.
let toolsPath = Ionide.ProjInfo.Init.init (DirectoryInfo <| Directory.GetCurrentDirectory())  None

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function
        | ErrorCode.HelpText -> None
        | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<ToolArgs>(programName = "fsharplint", errorHandler = errorHandler)
    let parseResults = parser.ParseCommandLine argv
    start parseResults toolsPath
    |> int
