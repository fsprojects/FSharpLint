module FSharpLint.Console.Program

open Argu
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

// Allowing underscores in union case names for proper Argu command line option formatting.
// fsharplint:disable UnionCasesNames
type private ToolArgs =
    | [<AltCommandLine("-f")>] Format of OutputFormat
    | [<AltCommandLine("-a")>] All_Files of bool
    | [<CliPrefix(CliPrefix.None)>] Lint of ParseResults<LintArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Format _ -> "Output format of the linter."
            | Lint _ -> "Runs FSharpLint against a file or a collection of files."
            | All_Files _ -> "If enabled, linter will produce output for files which have no warnings."

// TODO: investigate erroneous warning on this type definition
// fsharplint:disable UnionDefinitionIndentation
and private LintArgs =
    | [<MainCommand; Mandatory>] Target of target:string
    | [<AltCommandLine("-c")>] Release_Config of releaseConfig:string
    | [<AltCommandLine("-l")>] Lint_Config of lintConfig:string
    | File_Type of FileType
// fsharplint:enable UnionDefinitionIndentation
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "Input to lint."
            | File_Type _ -> "Input type the linter will run against. If this is not set, the file type will be inferred from the file extension."
            | Release_Config _ -> "Release config to use to parse files."
            | Lint_Config _ -> "Path to the config for the lint."
// fsharplint:enable UnionCasesNames

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

let private getParseFailureReason = function
    | ParseFile.FailedToParseFile failures ->
        let getFailureReason (x:FSharp.Compiler.SourceCodeServices.FSharpErrorInfo) =
            sprintf "failed to parse file %s, message: %s" x.FileName x.Message

        String.Join(", ", failures |> Array.map getFailureReason)
    | ParseFile.AbortedTypeCheck -> "Aborted type check."

let private start (arguments:ParseResults<ToolArgs>) =
    let mutable exitCode = 0

    let allFiles = arguments.TryGetResult All_Files |> Option.defaultValue false

    let output =
        match arguments.TryGetResult Format with
        | Some OutputFormat.MSBuild -> Output.MSBuild() :> Output.IOutput
        | Some OutputFormat.Standard
        | Some _
        | None -> Output.Standard(allFiles) :> Output.IOutput

    match arguments.GetSubCommand() with
    | Lint lintArgs ->
        let lintConfig = lintArgs.TryGetResult Lint_Config

        let configParam =
            match lintConfig with
            | Some configPath -> FromFile configPath
            | None -> Default

        let releaseConfig = lintArgs.TryGetResult Release_Config

        let lintParams =
            { CancellationToken = None
              Configuration = configParam
              HandleLintEvent = Some output.HandleLintEvent
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

            match lintResult with
            | Ok warnings -> if not (List.isEmpty warnings) then exitCode <- -1
            | Error _ -> exitCode <- -1

            output.HandleLintResult lintResult
        with
        | exn ->
            exitCode <- -1
            let target = if fileType = FileType.Source then "source" else target
            let message = sprintf "Lint failed while analysing %s.\nFailed with: %s\nStack trace: %s" target exn.Message exn.StackTrace
            output.HandleException message
    | _ -> ()

    exitCode

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<ToolArgs>(programName = "fsharplint", errorHandler = errorHandler)
    let parseResults = parser.ParseCommandLine argv
    start parseResults