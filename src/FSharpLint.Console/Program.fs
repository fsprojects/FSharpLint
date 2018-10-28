namespace FSharpLint.Console

module Program =

    open System
    open FSharpLint.Framework
    open FSharpLint.Application

    let private help () =
        printfn "-f <project.fsproj>        lint project"
        printfn "-sf <file.fs>              lint single file"
        printfn "-source 'let foo = 5'      lint source code"

    let private printException (e:Exception) =
        "Exception Message:" + Environment.NewLine +
        e.Message + Environment.NewLine +
        "Exception Stack Trace:" + Environment.NewLine +
        e.StackTrace + Environment.NewLine
        |> Console.WriteLine

    let private failedToParseFileError (file:string) parseException =
        let formatString = Resources.GetString("ConsoleFailedToParseFile")
        Console.WriteLine(String.Format(formatString, file))
        printException parseException

    let private parserProgress = function
        | Starting(_)
        | ReachedEnd(_) -> ()
        | Failed(file, parseException) ->
            failedToParseFileError file parseException

    let private runLint projectFile =
        let warningReceived = fun (error:LintWarning.Warning) -> 
            error.Info + Environment.NewLine + LintWarning.getWarningWithLocation error.Range error.Input
            |> Console.WriteLine

        let parseInfo =
            { CancellationToken = None
              ReceivedWarning = Some warningReceived
              Configuration = None }

        lintProject parseInfo projectFile (Some parserProgress)

    let private runLintOnFile pathToFile =
        let reportLintWarning (warning:LintWarning.Warning) =
            warning.Info + Environment.NewLine + LintWarning.getWarningWithLocation warning.Range warning.Input
            |> Console.WriteLine

        let parseInfo =
            { CancellationToken = None
              ReceivedWarning = Some reportLintWarning
              Configuration = None }

        lintFile parseInfo pathToFile

    let private runLintOnSource source =
        let getErrorMessage (range:Microsoft.FSharp.Compiler.Range.range) =
            let error = Resources.GetString("LintSourceError")
            String.Format(error, range.StartLine, range.StartColumn)

        let reportLintWarning (warning:LintWarning.Warning) = 
            warning.Info + Environment.NewLine + LintWarning.warningInfoLine getErrorMessage warning.Range warning.Input
            |> Console.WriteLine

        let parseInfo =
            { CancellationToken = None
              ReceivedWarning = Some reportLintWarning
              Configuration = None }

        lintSource parseInfo source

    type private Argument =
        | ProjectFile of string
        | SingleFile of string
        | Source of string
        | UnexpectedArgument of string

    let private parseArguments arguments =
        let rec parseArguments parsedArguments = function
            | "-f" :: argument :: remainingArguments -> 
                parseArguments (ProjectFile(argument) :: parsedArguments) remainingArguments
            | "-sf" :: argument :: remainingArguments ->
                parseArguments (SingleFile(argument) :: parsedArguments) remainingArguments
            | "-source" :: argument :: remainingArguments ->
                parseArguments (Source(argument) :: parsedArguments) remainingArguments
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
            | ProjectFile(_) | SingleFile(_) | Source(_) -> true 
            | _ -> false

        arguments |> List.exists isArgumentSpecifyingWhatToLint

    let private outputLintResult = function
        | LintResult.Success(_) -> Console.WriteLine(Resources.GetString("ConsoleFinished"))
        | LintResult.Failure(failure) -> Console.WriteLine(failure.Description)

    let private start projectFile =
        if System.IO.File.Exists(projectFile) then
            try runLint projectFile |> outputLintResult
            with
            | e -> 
                "Lint failed while analysing " + projectFile + 
                ".\nFailed with: " + e.Message + "\nStack trace: " + e.StackTrace
                |> Console.WriteLine
        else
            let formatString = Resources.GetString("ConsoleCouldNotFindFile")
            Console.WriteLine(String.Format(formatString, projectFile))

    let private startWithArguments arguments =
        arguments
        |> List.iter (function 
            | SingleFile(file) -> runLintOnFile file (Version(4, 0)) |> outputLintResult
            | Source(source) -> runLintOnSource source (Version(4, 0)) |> outputLintResult
            | ProjectFile(file) -> start file
            | UnexpectedArgument(_) -> ())
            
    [<EntryPoint>]
    let main argv =
        let parsedArguments = Array.toList argv |> parseArguments

        let argumentAreInvalid = 
            containsUnexpectedArgument parsedArguments || 
            not <| containsRequiredArguments parsedArguments

        if argumentAreInvalid then
            help()
        else
            startWithArguments parsedArguments

        0