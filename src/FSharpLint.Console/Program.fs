module FSharpLint.Console.Program

open Argu
open System.IO
open System
open FSharpLint.Framework
open FSharpLint.Application

/// Output format the linter will use.
type private OutputFormat =
    | Standard = 1
    | MSBuild = 2

/// File type the linter is running against.
type private FileType =
    | Project = 1
    | Solution = 2
    | File = 3
    | Source = 4

type private ToolArgs =
    | [<AltCommandLine("-f")>] Format of OutputFormat
    | [<CliPrefix(CliPrefix.None)>] Convert of ParseResults<ConvertArgs>
    | [<CliPrefix(CliPrefix.None)>] Lint of ParseResults<LintArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Format _ -> "Output format of the linter."
            | Convert _ -> "Converts an old-format XML config into a new format JSON config."
            | Lint _ -> "Runs FSharpLint against a file or a collection of files."

and private ConvertArgs =
    | Old_Config of oldConfig:string
    | [<MainCommand; Last>] New_Config of newConfig:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Old_Config _ -> "Path to the XML-style config to convert."
            | New_Config _ -> "Path to the file where the JSON-style config will be written."

and private LintArgs =
    | [<MainCommand; Mandatory>] Target of target:string
    | [<AltCommandLine("-c")>] Release_Config of releaseConfig:string
    | File_Type of FileType
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "Input to lint."
            | File_Type _ -> "Input type the linter will run against. If this is not set, the file type will be inferred from the file extension."
            | Release_Config _ -> "Release config to use to parse files."

let private parserProgress (output:Output.IOutput) = function
    | Starting file ->
        String.Format(Resources.GetString("ConsoleStartingFile"), file) |> output.WriteInfo
    | ReachedEnd (_, warnings) ->
        String.Format(Resources.GetString("ConsoleFinishedFile"), List.length warnings) |> output.WriteInfo
    | Failed (file, parseException) ->
        String.Format(Resources.GetString("ConsoleFailedToParseFile"), file) |> output.WriteError
        "Exception Message:" + Environment.NewLine +
            parseException.Message + Environment.NewLine +
            "Exception Stack Trace:" + Environment.NewLine +
            parseException.StackTrace + Environment.NewLine
        |> output.WriteError

let private convertConfig (xmlFile:string) (outputFile:string) =
    try
        let inputFile = File.ReadAllText xmlFile
        let jsonConfig = XmlConfiguration.convertToJson inputFile |> Configuration.serializeConfig
        File.WriteAllText(outputFile, jsonConfig)
        Choice1Of2 ()
    with
        | ex -> Choice2Of2 ex.Message

/// Infers the file type of the target based on its file extension.
let private inferFileType (target:string) =
    if target.EndsWith ".fs" || target.EndsWith ".fsx" then
        FileType.File
    else if target.EndsWith ".fsproj" then
        FileType.Project
    else if target.EndsWith ".sln" then
        FileType.Solution
    else
        FileType.Source

let private start (arguments:ParseResults<ToolArgs>) =
    let mutable exitCode = 0

    let (output:Output.IOutput) =
        match arguments.TryGetResult Format with
        | Some OutputFormat.MSBuild -> Output.MSBuildOutput() :> Output.IOutput
        | Some OutputFormat.Standard
        | Some _
        | None -> Output.StandardOutput() :> Output.IOutput

    let handleError (str:string) =
        output.WriteError str
        exitCode <- -1

    match arguments.GetSubCommand() with
    | Lint lintArgs ->

        let handleLintResult = function
            | LintResult.Success(warnings) ->
                String.Format(Resources.GetString("ConsoleFinished"), List.length warnings)
                |> output.WriteInfo
                if not (List.isEmpty warnings) then exitCode <- -1
            | LintResult.Failure(failure) ->
                handleError failure.Description

        let releaseConfig = lintArgs.TryGetResult Release_Config

        let lintParams =
            { CancellationToken = None
              ReceivedWarning = Some output.WriteWarning
              Configuration = None
              ReportLinterProgress = Some (parserProgress output)
              ReleaseConfiguration = releaseConfig }

        let target = lintArgs.GetResult Target
        let fileType = lintArgs.TryGetResult File_Type |> Option.defaultValue (inferFileType target)

        try
            let lintResult =
                match fileType with
                | FileType.File -> Lint.lintFile lintParams target
                | FileType.Source -> Lint.lintSource lintParams target
                | FileType.Solution -> Lint.lintSolution lintParams target
                | FileType.Project
                | _ -> Lint.lintProject lintParams target
            handleLintResult lintResult
        with
        | e ->
            let target = if fileType = FileType.Source then "source" else target
            sprintf "Lint failed while analysing %s.\nFailed with: %s\nStack trace: %s" target e.Message e.StackTrace
            |> handleError
    | Convert convertArgs ->
        let handleConversionResult xmlConfig outputFile = function
            | Choice1Of2 _ ->
                sprintf "Successfully converted config at '%s', saved to '%s'" xmlConfig outputFile
                |> output.WriteInfo
            | Choice2Of2 err ->
                sprintf "Failed to convert config at '%s', error: %s" xmlConfig err
                |> handleError

        let oldConfig = convertArgs.GetResult Old_Config
        let newConfig = convertArgs.GetResult New_Config
        convertConfig oldConfig newConfig |> handleConversionResult oldConfig newConfig
    | _ -> ()

    exitCode

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<ToolArgs>(programName = "fsharplint", errorHandler = errorHandler)
    let parseResults = parser.ParseCommandLine argv
    start parseResults