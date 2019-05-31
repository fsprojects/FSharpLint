namespace FSharpLint.Console

module Program =

    open System.IO
    open System
    open FSharpLint.Framework
    open FSharpLint.Application
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

    let private runLintOnProject projectFile =
        let parseInfo =
            { CancellationToken = None
              ReceivedWarning = Some writeLintWarning
              Configuration = None
              ReportLinterProgress = Some parserProgress }

        lintProject parseInfo projectFile

    let private runLintOnFile pathToFile =
        let parseInfo =
            { CancellationToken = None
              ReceivedWarning = Some writeLintWarning
              Configuration = None
              ReportLinterProgress = Some parserProgress }

        lintFile parseInfo pathToFile

    let private runLintOnSource source =
        let getErrorMessage (range:FSharp.Compiler.Range.range) =
            let error = Resources.GetString("LintSourceError")
            String.Format(error, range.StartLine, range.StartColumn)

        let reportLintWarning (warning:LintWarning.Warning) = 
            warning.Info + Environment.NewLine + LintWarning.warningInfoLine getErrorMessage warning.Range warning.Input
            |> writeWarningLine

        let parseInfo =
            { CancellationToken = None
              ReceivedWarning = Some reportLintWarning
              Configuration = None
              ReportLinterProgress = None }

        lintSource parseInfo source
        
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
        | SingleFile of string
        | Source of string
        | ConvertConfig of string * string
        | UnexpectedArgument of string

        override this.ToString() =
            match this with
            | ProjectFile(f) -> "project " + f
            | SingleFile(f) -> "source file " + f
            | Source(_) -> "source code"
            | ConvertConfig(xmlConfig, outputFile) -> "config " + xmlConfig + " " + outputFile
            | UnexpectedArgument(_) -> "unexpected argument"

    let private parseArguments arguments =
        let rec parseArguments parsedArguments = function
            | "-f" :: argument :: remainingArguments -> 
                parseArguments (ProjectFile(argument) :: parsedArguments) remainingArguments
            | "-sf" :: argument :: remainingArguments ->
                parseArguments (SingleFile(argument) :: parsedArguments) remainingArguments
            | "-source" :: argument :: remainingArguments ->
                parseArguments (Source(argument) :: parsedArguments) remainingArguments
             | "-convert" :: xmlConfig :: outputFile :: remainingArguments ->
                parseArguments (ConvertConfig(xmlConfig, outputFile) :: parsedArguments) remainingArguments               
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
            | ProjectFile(_) | SingleFile(_) | Source(_) | ConvertConfig(_) -> true 
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
        
        arguments
        |> List.iter (fun arg ->
            try
                match arg with
                | SingleFile(file) | ProjectFile(file) when not (IO.File.Exists file) ->
                    let formatString = Resources.GetString("ConsoleCouldNotFindFile")
                    String.Format(formatString, file) |> handleError
                | SingleFile(file) -> runLintOnFile file |> handleLintResult
                | Source(source) -> runLintOnSource source |> handleLintResult
                | ProjectFile(projectFile) -> runLintOnProject projectFile |> handleLintResult
                | ConvertConfig(xmlConfig, outputFile) -> convertConfig xmlConfig outputFile |> handleConversionResult xmlConfig outputFile
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
