namespace FSharpLint.Console

module Program =

    open System.IO
    open System
    open FSharp.Compiler.Range
    open FSharpLint.Framework
    open FSharpLint.Application

    type OutputFormat =
        // Standard FSharpLint output
        | Standard
        // Output format which is parseable by MSBuild
        | MSBuild

    let mutable outputFormat = OutputFormat.Standard

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

    let private writeInfoLine (str:string) =
        match outputFormat with
        | Standard -> writeLine str ConsoleColor.White Console.Out
        | MSBuild -> ()

    let private writeWarningLine (warning:Suggestion.LintWarning) =
        match outputFormat with
        | Standard ->
            let highlightedErrorText = highlightErrorText warning.Details.Range (getErrorMessage warning.Details.Range)
            let str = warning.Details.Message + Environment.NewLine + highlightedErrorText + Environment.NewLine + warning.ErrorText
            writeLine str ConsoleColor.Yellow Console.Out
            String.replicate 80 "-" |> writeInfoLine
        | MSBuild ->
            sprintf "%s(%d,%d,%d,%d):FSharpLint warning %s: %s"
                <| warning.FilePath
                <| warning.Details.Range.StartLine
                <| warning.Details.Range.StartColumn
                <| warning.Details.Range.EndLine
                <| warning.Details.Range.EndColumn
                <| warning.RuleIdentifier
                <| warning.Details.Message
            |> Console.Out.WriteLine

    let private writeErrorLine (str:string) =
        match outputFormat with
        | Standard -> writeLine str ConsoleColor.Red Console.Error
        | MSBuild -> ()

    let private help () =
        writeInfoLine "-f <project.fsproj>                lint project"
        writeInfoLine "-sol <solution.sln>                lint solution"
        writeInfoLine "-sf <file.fs>                      lint single file"
        writeInfoLine "-source 'let foo = 5'              lint source code"
        writeInfoLine "-convert <xmlConfig> <outputFile>  convert old XML config to JSON"
        writeInfoLine "-format                            output format (standard/msbuild)"

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

    type private Argument =
        | ProjectFile of string
        | SolutionFile of string
        | SingleFile of string
        | Source of string
        | ConvertConfig of string * string
        | ReleaseConfig of string
        | OutputFormat of string
        | UnexpectedArgument of string

        override this.ToString() =
            match this with
            | ProjectFile(f) -> "project " + f
            | SolutionFile(f) -> "solution " + f
            | SingleFile(f) -> "source file " + f
            | Source(_) -> "source code"
            | ConvertConfig(xmlConfig, outputFile) -> "config " + xmlConfig + " " + outputFile
            | ReleaseConfig (config) -> "releaseConfig " + config
            | OutputFormat format -> "format " + format
            | UnexpectedArgument(_) -> "unexpected argument"

    let private parseArguments arguments =
        let rec parseArguments parsedArguments = function
            | "-f" :: argument :: remainingArguments ->
                parseArguments (ProjectFile(argument) :: parsedArguments) remainingArguments
            | "-sol" :: argument :: remainingArguments ->
                parseArguments (SolutionFile(argument) :: parsedArguments) remainingArguments
            | "-sf" :: argument :: remainingArguments ->
                parseArguments (SingleFile(argument) :: parsedArguments) remainingArguments
            | "-source" :: argument :: remainingArguments ->
                parseArguments (Source(argument) :: parsedArguments) remainingArguments
            | "-convert" :: xmlConfig :: outputFile :: remainingArguments ->
                parseArguments (ConvertConfig(xmlConfig, outputFile) :: parsedArguments) remainingArguments
            | "-c" :: releaseConfig :: remainingArguments ->
                parseArguments (ReleaseConfig (releaseConfig) :: parsedArguments) remainingArguments
            | "-format" :: outputFormat :: remainingArguments ->
                parseArguments (OutputFormat outputFormat :: parsedArguments) remainingArguments
            | [] -> parsedArguments
            | argument :: _ ->  [UnexpectedArgument(argument)]

        parseArguments [] arguments

    let private containsUnexpectedArgument arguments =
        let isUnexpectedArgument = function
            | UnexpectedArgument(_) -> true
            | _ -> false

        arguments |> List.exists isUnexpectedArgument

    let private containsRequiredArguments arguments =
        let isArgumentSpecifyingWhatToLint = function
            | ProjectFile(_) | SolutionFile(_) | SingleFile(_) | Source(_) | ConvertConfig(_) -> true
            | _ -> false

        arguments |> List.exists isArgumentSpecifyingWhatToLint

    let private startWithArguments arguments =
        let mutable exitCode = 0

        let handleError (str:string) =
            writeErrorLine str
            exitCode <- -1

        let handleLintResult = function
            | LintResult.Success(warnings) ->
                String.Format(Resources.GetString("ConsoleFinished"), List.length warnings) |> writeInfoLine
                if not (List.isEmpty warnings) then exitCode <- -1
            | LintResult.Failure(failure) ->
                handleError failure.Description

        let handleConversionResult xmlConfig outputFile = function
            | Choice1Of2 _ ->
                (sprintf "Successfully converted config at '%s', saved to '%s'" xmlConfig outputFile) |> writeInfoLine
            | Choice2Of2 err ->
                (sprintf "Failed to convert config at '%s', error: %s" xmlConfig err) |> handleError

        let releaseConfig = arguments |> List.tryPick (function | ReleaseConfig config -> Some config | _ -> None)

        let lintParams =
            { CancellationToken = None
              ReceivedWarning = Some writeWarningLine
              Configuration = None
              ReportLinterProgress = Some parserProgress
              ReleaseConfiguration = releaseConfig }

        // Set output mode.
        arguments
        |> List.iter (function
                | OutputFormat "msbuild" -> outputFormat <- OutputFormat.MSBuild
                | _ -> ())

        arguments
        |> List.iter (fun arg ->
            try
                match arg with
                | SingleFile file
                | ProjectFile file
                | SolutionFile file when not (IO.File.Exists file) ->
                    let formatString = Resources.GetString("ConsoleCouldNotFindFile")
                    String.Format(formatString, file) |> handleError
                | SingleFile file -> runLintOnFile lintParams file |> handleLintResult
                | Source source -> runLintOnSource lintParams source |> handleLintResult
                | ProjectFile projectFile -> runLintOnProject lintParams projectFile |> handleLintResult
                | ConvertConfig (xmlConfig, outputFile) -> convertConfig xmlConfig outputFile |> handleConversionResult xmlConfig outputFile
                | SolutionFile solutionFile -> runLintOnSolution lintParams solutionFile |> handleLintResult
                | OutputFormat _
                | ReleaseConfig _
                | UnexpectedArgument _ -> ()
            with
            | e ->
                "Lint failed while analysing " + string arg + "." + Environment.NewLine +
                    "Failed with: " + e.Message + Environment.NewLine +
                    "Stack trace:" + e.StackTrace
                |> handleError)
        exitCode

    [<EntryPoint>]
    let main argv =
        let parsedArguments = Array.toList argv |> parseArguments

        let argumentAreInvalid =
            containsUnexpectedArgument parsedArguments ||
            not <| containsRequiredArguments parsedArguments

        if argumentAreInvalid then
            help()
            -1
        else
            startWithArguments parsedArguments
