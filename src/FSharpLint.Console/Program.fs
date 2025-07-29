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

type ExitCode = 
    | Error = -1
    | Success = 0
    | NoSuchRuleName = 1
    | NoFix = 2

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
            | Fix _ -> "Apply fixes for specified rule name or names (comma separated)."
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
            | Fix_Target _ -> "Rule name to be applied with fix and input to lint."
            | Fix_File_Type _ -> fileTypeHelp
// fsharplint:enable UnionCasesNames

type private LintingArgs =
    {
        FileType: FileType
        LintParams: OptionalLintParameters
        Target: string
        ToolsPath: Ionide.ProjInfo.Types.ToolsPath
        ShouldFix: bool
        MaybeRuleName: string option
    }

let private parserProgress (output:Output.IOutput) = function
    | Starting file ->
        String.Format(Resources.GetString("ConsoleStartingFile"), file) |> output.WriteInfo
    | ReachedEnd (_, warnings) ->
        String.Format(Resources.GetString("ConsoleFinishedFile"), List.length warnings) |> output.WriteInfo
    | Failed (file, parseException) ->
        String.Format(Resources.GetString("ConsoleFailedToParseFile"), file) |> output.WriteError
        output.WriteError
            $"Exception Message:{Environment.NewLine}{parseException.Message}{Environment.NewLine}Exception Stack Trace:{Environment.NewLine}{parseException.StackTrace}{Environment.NewLine}"

/// Infers the file type of the target based on its file extension.
let internal inferFileType (target:string) =
    if target.EndsWith ".fs" || target.EndsWith ".fsx" then
        FileType.File
    else if target.EndsWith ".fsproj" then
        FileType.Project
    else if target.EndsWith ".slnx" || target.EndsWith ".slnf" || target.EndsWith ".sln" then
        FileType.Solution
    else
        FileType.Source

// can't extract inner functions because they modify exitCode variable
// fsharplint:disable MaxLinesInFunction
let private start (arguments:ParseResults<ToolArgs>) (toolsPath:Ionide.ProjInfo.Types.ToolsPath) =
    let mutable exitCode = ExitCode.Success

    let output =
        match arguments.TryGetResult Format with
        | Some OutputFormat.MSBuild -> Output.MSBuildOutput() :> Output.IOutput
        | Some OutputFormat.Standard
        | Some _
        | None -> Output.StandardOutput() :> Output.IOutput

    if arguments.Contains ToolArgs.Version then
        let version =
            Assembly.GetExecutingAssembly().GetCustomAttributes false
            |> Seq.pick (function | :? AssemblyInformationalVersionAttribute as aiva -> Some aiva.InformationalVersion | _ -> None)
        output.WriteInfo $"Current version: {version}"
        Environment.Exit 0

    let handleError (status:ExitCode) (str:string) =
        output.WriteError str
        exitCode <- status

    let outputWarnings (warnings: List<Suggestion.LintWarning>) =
        String.Format(Resources.GetString "ConsoleFinished", List.length warnings)
            |> output.WriteInfo
    
    let handleLintResult = function
        | LintResult.Success warnings ->
            outputWarnings warnings
            if List.isEmpty warnings |> not then 
               exitCode <- ExitCode.Error
        | LintResult.Failure failure -> handleError ExitCode.Error failure.Description
    
    let handleFixResult (target: string) (ruleName: string) = function
        | LintResult.Success warnings ->
            String.Format(Resources.GetString "ConsoleApplyingFixFile", target) |> output.WriteInfo
            let increment = 1
            let noFixIncrement = 0

            let countFixes (element: Suggestion.LintWarning) =
                let sourceCode = File.ReadAllText element.FilePath
                if String.Equals(ruleName, element.RuleName, StringComparison.InvariantCultureIgnoreCase) then
                    match element.Details.Fix with
                    | Some lazyFix ->
                        match lazyFix.Force() with
                        | Some fix ->
                            let updatedSourceCode = 
                                let builder = StringBuilder(sourceCode.Length + fix.ToText.Length)
                                let firstPart = 
                                    sourceCode.AsSpan(
                                        0,
                                        (ExpressionUtilities.findPos fix.FromRange.Start sourceCode).Value
                                    )
                                let secondPart = 
                                    sourceCode.AsSpan
                                        (ExpressionUtilities.findPos fix.FromRange.End sourceCode).Value
                                builder
                                    .Append(firstPart)
                                    .Append(fix.ToText)
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

            let countFix = 
                warnings |> List.sumBy countFixes
            outputWarnings warnings

            if countFix > 0 then
                exitCode <- ExitCode.Success
            else
                exitCode <- ExitCode.NoFix

        | LintResult.Failure failure -> handleError ExitCode.Error failure.Description

    let linting (args: LintingArgs) =
        try
            let lintResult =
                match args.FileType with
                | FileType.File -> Lint.lintFile args.LintParams args.Target
                | FileType.Source -> Lint.lintSource args.LintParams args.Target
                | FileType.Solution -> Lint.lintSolution args.LintParams args.Target toolsPath
                | FileType.Project
                | _ -> Lint.lintProject args.LintParams args.Target toolsPath
            if args.ShouldFix then
                match args.MaybeRuleName with
                | Some ruleName -> handleFixResult args.Target ruleName lintResult
                | None -> exitCode <- ExitCode.NoSuchRuleName
            else
                handleLintResult lintResult
        with
        | exn ->
            let target = if args.FileType = FileType.Source then "source" else args.Target
            $"Lint failed while analysing %s{target}.{Environment.NewLine}Failed with: %s{exn.Message}{Environment.NewLine}Stack trace: {exn.StackTrace}"
            |> handleError ExitCode.Error

    let getParams config =
        let paramConfig =
            match config with
            | Some configPath -> FromFile configPath
            | None -> Default

        { CancellationToken = None
          ReceivedWarning = Some output.WriteWarning
          Configuration = paramConfig
          ReportLinterProgress = parserProgress output |> Some }

    let applyLint (lintArgs: ParseResults<LintArgs>) =
        let lintConfig = lintArgs.TryGetResult Lint_Config

        let lintParams = getParams lintConfig
        let target = lintArgs.GetResult Target
        let fileType = lintArgs.TryGetResult File_Type |> Option.defaultValue (inferFileType target)

        linting 
            { FileType = fileType
              LintParams = lintParams
              Target = target
              ToolsPath = toolsPath
              ShouldFix = false
              MaybeRuleName = None }

    let applyFix (fixArgs: ParseResults<FixArgs>) =
        let fixParams = getParams None
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
            linting 
                { FileType = fileType
                  LintParams = fixParams
                  Target = target
                  ToolsPath = toolsPath
                  ShouldFix = true 
                  MaybeRuleName = Some ruleName }
        else
            sprintf "Rule '%s' does not exist." ruleName |> (handleError ExitCode.NoSuchRuleName)

    match arguments.GetSubCommand() with
    | Lint lintArgs -> applyLint lintArgs
    | Fix fixArgs -> applyFix fixArgs
    | _ -> ()

    int exitCode
// fsharplint:enable MaxLinesInFunction
    
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
