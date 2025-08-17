﻿module FSharpLint.Console.Program

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

// Allowing underscores in union case names for proper Argu command line option formatting.
// fsharplint:disable UnionCasesNames
type private ToolArgs =
    | [<AltCommandLine("-f")>] Format of OutputFormat
    | [<CliPrefix(CliPrefix.None)>] Lint of ParseResults<LintArgs>
    | [<Unique>] Report of string
    | [<Unique>] Code_Root of string
    | Version
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Format _ -> "Output format of the linter."
            | Lint _ -> "Runs FSharpLint against a file or a collection of files."
            | Report _ -> "Write the result messages to a (sarif) report file."
            | Code_Root _ -> "Root of the current code repository, used in the sarif report to construct the relative file path. The current working directory is used by default."
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

let private start (arguments:ParseResults<ToolArgs>) (toolsPath:Ionide.ProjInfo.Types.ToolsPath) =
    let mutable exitCode = 0

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

    let handleError (str:string) =
        output.WriteError str
        exitCode <- -1

    let reportPath = arguments.TryGetResult Report
    let codeRoot = arguments.TryGetResult Code_Root

    match arguments.GetSubCommand() with
    | Lint lintArgs ->

        let handleLintResult = function
            | LintResult.Success(warnings) ->
                String.Format(Resources.GetString("ConsoleFinished"), List.length warnings)
                |> output.WriteInfo

                reportPath 
                |> Option.iter (fun report -> Sarif.writeReport warnings codeRoot report output)

                if not (List.isEmpty warnings) then exitCode <- -1
            | LintResult.Failure(failure) ->
                handleError failure.Description

        let lintConfig = lintArgs.TryGetResult Lint_Config

        let configParam =
            match lintConfig with
            | Some configPath -> FromFile configPath
            | None -> Default


        let lintParams =
            { CancellationToken = None
              ReceivedWarning = Some output.WriteWarning
              Configuration = configParam
              ReportLinterProgress = Some (parserProgress output) }

        let target = lintArgs.GetResult Target
        let fileType = lintArgs.TryGetResult File_Type |> Option.defaultValue (inferFileType target)

        try
            let lintResult =
                match fileType with
                | FileType.File -> Lint.lintFile lintParams target
                | FileType.Source -> Lint.lintSource lintParams target
                | FileType.Solution -> Lint.lintSolution lintParams target toolsPath
                | FileType.Project
                | _ -> Lint.lintProject lintParams target toolsPath
            handleLintResult lintResult
        with
        | exn ->
            let target = if fileType = FileType.Source then "source" else target
            handleError
                $"Lint failed while analysing %s{target}.{Environment.NewLine}Failed with: %s{exn.Message}{Environment.NewLine}Stack trace: {exn.StackTrace}"
    | _ -> ()

    exitCode

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
