(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

namespace FSharpLint.Console

module Program =

    open FSharpLint.Framework
    open FSharpLint.Application

    let private help () =
        System.Console.WriteLine(Resources.GetString("ConsoleHelp"))

    let private printException (e:System.Exception) =
        System.Console.WriteLine("Exception Message:")
        System.Console.WriteLine(e.Message)
        System.Console.WriteLine("Exception Stack Trace:")
        System.Console.WriteLine(e.StackTrace)

    let private failedToParseFileError (file:string) parseException =
        let formatString = Resources.GetString("ConsoleFailedToParseFile")
        System.Console.WriteLine(System.String.Format(formatString, file))
        printException parseException

    let private parserProgress = function
        | Starting(_)
        | ReachedEnd(_) -> ()
        | Failed(file, parseException) ->
            failedToParseFileError file parseException

    let private runLint projectFile =
        let warningReceived = fun (error:LintWarning.Warning) -> 
            let output = error.Info + System.Environment.NewLine + LintWarning.getWarningWithLocation error.Range error.Input
            System.Console.WriteLine output

        let parseInfo =
            {
                FinishEarly = None
                ReceivedWarning = Some warningReceived
                Configuration = None
            }

        lintProject parseInfo projectFile (Some parserProgress)

    let private runLintOnFile pathToFile =
        let reportLintWarning (warning:LintWarning.Warning) =
            let output = warning.Info + System.Environment.NewLine + LintWarning.getWarningWithLocation warning.Range warning.Input
            System.Console.WriteLine(output)

        let parseInfo =
            {
                FinishEarly = None
                ReceivedWarning = Some reportLintWarning
                Configuration = None
            }

        lintFile parseInfo pathToFile

    let private runLintOnSource source =
        let getErrorMessage (range:Microsoft.FSharp.Compiler.Range.range) =
            let error = FSharpLint.Framework.Resources.GetString("LintSourceError")
            System.String.Format(error, range.StartLine, range.StartColumn)

        let reportLintWarning (warning:LintWarning.Warning) = 
            let output = warning.Info + System.Environment.NewLine + LintWarning.warningInfoLine getErrorMessage warning.Range warning.Input
            System.Console.WriteLine(output)

        let parseInfo =
            {
                FinishEarly = None
                ReceivedWarning = Some reportLintWarning
                Configuration = None
            }

        lintSource parseInfo source

    let private getParseFailureReason = function
        | ParseFile.FailedToParseFile(failures) ->
            let getFailureReason (x:Microsoft.FSharp.Compiler.FSharpErrorInfo) =
                sprintf "failed to parse file %s, message: %s" x.FileName x.Message

            System.String.Join(", ", failures |> Array.map getFailureReason)
        | ParseFile.AbortedTypeCheck ->
            "Aborted type check."

    let private printFailedDescription = function
        | ProjectFileCouldNotBeFound(projectPath) ->
            let formatString = Resources.GetString("ConsoleProjectFileCouldNotBeFound")
            System.Console.WriteLine(System.String.Format(formatString, projectPath))

        | MSBuildFailedToLoadProjectFile(projectPath, e) ->
            let formatString = Resources.GetString("ConsoleMSBuildFailedToLoadProjectFile")
            System.Console.WriteLine(System.String.Format(formatString, projectPath, e.Message))

        | FailedToLoadConfig(message) ->
            let formatString = Resources.GetString("ConsoleFailedToLoadConfig")
            System.Console.WriteLine(System.String.Format(formatString, message))

        | RunTimeConfigError ->
            System.Console.WriteLine(Resources.GetString("ConsoleRunTimeConfigError"))

        | FailedToParseFile(failure) ->
            System.Console.WriteLine(
                "Lint failed to parse a file. Failed with: " + 
                getParseFailureReason failure)

        | FailedToParseFilesInProject(failures) -> 
            System.Console.WriteLine(
                "Lint failed to parse files. Failed with: " + 
                System.String.Join("\n", failures |> List.map getParseFailureReason))

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
            | [] -> 
                parsedArguments
            | argument :: _ -> 
                [UnexpectedArgument(argument)]

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
        | LintResult.Success(_) ->
            System.Console.WriteLine(Resources.GetString("ConsoleFinished"))
        | LintResult.Failure(error) ->
            printFailedDescription error

    let private start projectFile =
        if System.IO.File.Exists(projectFile) then
            try
                runLint projectFile |> outputLintResult
            with
                | e -> 
                    System.Console.WriteLine("Lint failed while analysing " + projectFile + ".\nFailed with: " + e.Message + "\nStack trace: " + e.StackTrace)

        else
            let formatString = Resources.GetString("ConsoleCouldNotFindFile")
            System.Console.WriteLine(System.String.Format(formatString, projectFile))

    let private startWithArguments arguments =
        arguments
            |> List.iter (function 
                | SingleFile(file) -> runLintOnFile file (System.Version(4, 0)) |> outputLintResult
                | Source(source) -> runLintOnSource source (System.Version(4, 0)) |> outputLintResult
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