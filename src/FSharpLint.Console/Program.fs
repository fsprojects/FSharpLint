module FSharpLint.Console.Program

open Argu
open System.IO
open System
open FSharp.Compiler.Range
open FSharpLint.Framework
open FSharpLint.Application

/// Output format the linter will use.
type OutputFormat =
    | Standard = 1
    | MSBuild = 2

/// File type the linter is running against.
type FileType =
    | Project = 1
    | Solution = 2
    | File = 3
    | Source = 4

type ToolArgs =
    | [<CliPrefix(CliPrefix.None)>] Convert of ParseResults<ConvertArgs>
    | [<CliPrefix(CliPrefix.None)>] Lint of ParseResults<LintArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Convert _ -> "Converts an old-format XML config into a new format JSON config."
            | Lint _ -> "Runs FSharpLint against a file or a collection of files."

and ConvertArgs =
    | Old_Config of oldConfig:string
    | [<MainCommand; Last>] New_Config of newConfig:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Old_Config _ -> "Path to the XML-style config to convert."
            | New_Config _ -> "Path to the file where the JSON-style config will be written."

and LintArgs =
    | [<MainCommand; Mandatory>] Target of target:string
    | [<AltCommandLine("-c")>] Release_Config of releaseConfig:string
    | File_Type of fileType:FileType
    | Format of format:OutputFormat
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "Input to lint."
            | File_Type _ -> "Input type the linter will run against. If this is not set, the file type will be inferred from the file extension."
            | Release_Config _ -> "Release config to use to parse files."
            | Format _ -> "Output format of the linter."

let private highlightErrorText (range:range) (errorLine:string) =
    let highlightColumnLine =
        if String.length errorLine = 0 then "^"
        else
            errorLine
            |> Seq.mapi (fun i _ -> if i = range.StartColumn then "^" else " ")
            |> Seq.reduce (+)
    errorLine + Environment.NewLine + highlightColumnLine

let getErrorMessage (range:FSharp.Compiler.Range.range) =
    let error = Resources.GetString("LintSourceError")
    String.Format(error, range.StartLine, range.StartColumn)

let private writeLine (str:string) (color:ConsoleColor) (writer:IO.TextWriter) =
    let originalColour = Console.ForegroundColor
    Console.ForegroundColor <- color
    writer.WriteLine str
    Console.ForegroundColor <- originalColour

let mutable outputFormat = OutputFormat.Standard

let private writeInfoLine (str:string) =
    match outputFormat with
    | OutputFormat.MSBuild -> ()
    | OutputFormat.Standard
    | _ -> writeLine str ConsoleColor.White Console.Out

let private writeWarningLine (warning:Suggestion.LintWarning) =
    match outputFormat with
    | OutputFormat.MSBuild ->
        sprintf "%s(%d,%d,%d,%d):FSharpLint warning %s: %s"
            <| warning.FilePath
            <| warning.Details.Range.StartLine
            <| warning.Details.Range.StartColumn
            <| warning.Details.Range.EndLine
            <| warning.Details.Range.EndColumn
            <| warning.RuleIdentifier
            <| warning.Details.Message
        |> Console.Out.WriteLine
    | OutputFormat.Standard
    | _ ->
         let highlightedErrorText = highlightErrorText warning.Details.Range (getErrorMessage warning.Details.Range)
         let str = warning.Details.Message + Environment.NewLine + highlightedErrorText + Environment.NewLine + warning.ErrorText
         writeLine str ConsoleColor.Yellow Console.Out
         String.replicate 80 "-" |> writeInfoLine

let private writeErrorLine (str:string) =
    match outputFormat with
    | OutputFormat.MSBuild -> ()
    | OutputFormat.Standard
    | _ -> writeLine str ConsoleColor.Red Console.Error

let private parserProgress = function
    | Starting(file) ->
        String.Format(Resources.GetString("ConsoleStartingFile"), file) |> writeInfoLine
    | ReachedEnd(_, warnings) ->
        String.Format(Resources.GetString("ConsoleFinishedFile"), List.length warnings) |> writeInfoLine
    | Failed(file, parseException) ->
        String.Format(Resources.GetString("ConsoleFailedToParseFile"), file) |> writeErrorLine
        "Exception Message:" + Environment.NewLine +
            parseException.Message + Environment.NewLine +
            "Exception Stack Trace:" + Environment.NewLine +
            parseException.StackTrace + Environment.NewLine
        |> writeErrorLine

let private runLintOnProject lintParams projectFile =
    lintProject lintParams projectFile

let private runLintOnSolution lintParams solutionFile =
    lintSolution lintParams solutionFile

let private runLintOnFile lintParams pathToFile =
    lintFile lintParams pathToFile

let private runLintOnSource lintParams source =
    let lintParams = { lintParams with ReceivedWarning = Some writeWarningLine }

    lintSource lintParams source

let private convertConfig (xmlFile:string) (outputFile:string) =
    try
        let inputFile = File.ReadAllText xmlFile
        let jsonConfig = XmlConfiguration.convertToJson inputFile |> ConfigurationManager.serializeConfig
        File.WriteAllText(outputFile, jsonConfig)
        Choice1Of2 ()
    with
        | ex -> Choice2Of2 ex.Message

/// Infers the file type of the target based on its file extension.
let inferFileType (target:string) =
    if target.EndsWith ".fs" || target.EndsWith ".fsx" then
        FileType.File
    else if target.EndsWith ".fsproj" then
        FileType.Project
    else if target.EndsWith ".sln" then
        FileType.Solution
    else
        FileType.Source

let private start (parser:ArgumentParser<ToolArgs>) (arguments:ParseResults<ToolArgs>) =
    let mutable exitCode = 0

    let handleError (str:string) =
        writeErrorLine str
        exitCode <- -1

    match arguments.TryGetSubCommand() with
    | Some (Lint lintArgs) ->
        let handleLintResult = function
            | LintResult.Success(warnings) ->
                String.Format(Resources.GetString("ConsoleFinished"), List.length warnings) |> writeInfoLine
                if not (List.isEmpty warnings) then exitCode <- -1
            | LintResult.Failure(failure) ->
                handleError failure.Description

        let releaseConfig = lintArgs.TryGetResult Release_Config
        outputFormat <- lintArgs.TryGetResult Format |> Option.defaultValue OutputFormat.Standard

        let lintParams =
            { CancellationToken = None
              ReceivedWarning = Some writeWarningLine
              Configuration = None
              ReportLinterProgress = Some parserProgress
              ReleaseConfiguration = releaseConfig }

        let target = lintArgs.GetResult Target
        let fileType = lintArgs.TryGetResult File_Type |> Option.defaultValue (inferFileType target)

        try
            let lintResult =
                match fileType with
                | FileType.File -> lintFile lintParams target
                | FileType.Source -> lintSource lintParams target
                | FileType.Solution -> lintSolution lintParams target
                | FileType.Project
                | _ -> lintProject lintParams target
            handleLintResult lintResult
        with
        | e ->
            let target = if fileType = FileType.Source then "source" else target
            sprintf "Lint failed while analysing %s.\nFailed with: %s\nStack trace: %s" target e.Message e.StackTrace
            |> handleError
    | Some (Convert convertArgs) ->
        let handleConversionResult xmlConfig outputFile = function
            | Choice1Of2 _ ->
                (sprintf "Successfully converted config at '%s', saved to '%s'" xmlConfig outputFile) |> writeInfoLine
            | Choice2Of2 err ->
                (sprintf "Failed to convert config at '%s', error: %s" xmlConfig err) |> handleError

        match (convertArgs.TryGetResult Old_Config, convertArgs.TryGetResult New_Config) with
        | (Some oldConfig, Some newConfig) ->
            convertConfig oldConfig newConfig
            |> handleConversionResult oldConfig newConfig
        | _ ->
            // TODO: conversion usage
            ()
    | None ->
        printfn "%s" (parser.PrintUsage())
        exitCode <- -1

    exitCode

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<ToolArgs>(programName = "fsharplint", errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    start parser results
