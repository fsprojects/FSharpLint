namespace FSharpLint.Console

module Program =

    open System.IO
    open System
    open FSharpLint.Framework
    open FSharpLint.Application

    let private writeLine (str:string) (color:ConsoleColor) (writer:IO.TextWriter) =
        let originalColour = Console.ForegroundColor
        Console.ForegroundColor <- color
        writer.WriteLine str
        Console.ForegroundColor <- originalColour
        
    let private writeInfoLine (str:string) = 
        writeLine str ConsoleColor.White Console.Out
        
    let private writeWarningLine (str:string) = 
        writeLine str ConsoleColor.Yellow Console.Out
        
    let private writeErrorLine (str:string) = 
        writeLine str ConsoleColor.Red Console.Error
        
    let private help () =
        writeInfoLine "-f <project.fsproj>                lint project"
        writeInfoLine "-sol <solution.sln>                lint solution"
        writeInfoLine "-sf <file.fs>                      lint single file"
        writeInfoLine "-source 'let foo = 5'              lint source code"
        writeInfoLine "-convert <xmlConfig> <outputFile>  convert old XML config to JSON"
        
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

    let private writeLintWarning (error:LintWarning.Warning) =
        error.Info + Environment.NewLine + LintWarning.getWarningWithLocation error.Range error.Input
        |> writeWarningLine
        String.replicate 80 "-" |> writeInfoLine

    let private runLintOnProject lintParams projectFile =
        lintProject lintParams projectFile
        
    let private runLintOnSolution lintParams solutionFile =
        lintSolution lintParams solutionFile

    let private runLintOnFile lintParams pathToFile =
        lintFile lintParams pathToFile

    let private runLintOnSource lintParams source =
        let getErrorMessage (range:FSharp.Compiler.Range.range) =
            let error = Resources.GetString("LintSourceError")
            String.Format(error, range.StartLine, range.StartColumn)

        let reportLintWarning (warning:LintWarning.Warning) = 
            warning.Info + Environment.NewLine + LintWarning.warningInfoLine getErrorMessage warning.Range warning.Input
            |> writeWarningLine

        let lintParams = { lintParams with ReceivedWarning = Some reportLintWarning }

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
        | UnexpectedArgument of string

        override this.ToString() =
            match this with
            | ProjectFile(f) -> "project " + f
            | SolutionFile(f) -> "solution " + f
            | SingleFile(f) -> "source file " + f
            | Source(_) -> "source code"
            | ConvertConfig(xmlConfig, outputFile) -> "config " + xmlConfig + " " + outputFile
            | ReleaseConfig (config) -> "releaseConfig " + config
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
              ReceivedWarning = Some writeLintWarning
              Configuration = None
              ReportLinterProgress = Some parserProgress
              ReleaseConfiguration = releaseConfig }
        
        arguments
        |> List.iter (fun arg ->
            try
                match arg with
                | SingleFile(file) | ProjectFile(file) | SolutionFile(file) when not (IO.File.Exists file) ->
                    let formatString = Resources.GetString("ConsoleCouldNotFindFile")
                    String.Format(formatString, file) |> handleError
                | SingleFile(file) -> runLintOnFile lintParams file |> handleLintResult
                | Source(source) -> runLintOnSource lintParams source |> handleLintResult
                | ProjectFile(projectFile) -> runLintOnProject lintParams projectFile |> handleLintResult
                | ConvertConfig(xmlConfig, outputFile) -> convertConfig xmlConfig outputFile |> handleConversionResult xmlConfig outputFile
                | SolutionFile(solutionFile) -> runLintOnSolution lintParams solutionFile |> handleLintResult
                | ReleaseConfig(_)
                | UnexpectedArgument(_) -> ()
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
