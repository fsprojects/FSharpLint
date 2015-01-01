module FSharpLint.FAKE

open FSharpLint.Application
open FSharpLint.Framework

type LintOptions =
    {
        /// Function that when returns true cancels the parsing of the project, useful for cancellation tokens etc.
        FinishEarly: System.Func<bool>

        /// Callback that's called at the start and end of parsing each file (or when a file fails to be parsed).
        Progress: System.Action<Application.RunLint.ParserProgress>

        /// Callback that's called when a lint error is detected.
        ErrorReceived: System.Action<ErrorHandling.Error>

        /// Optionally force the lint to lookup FSharp.Core.dll from this directory.
        FSharpCoreDirectory: string option
    }

/// the default never finishes early
let private defaultFinishEarly _ = false 

let private printException (e:System.Exception) =
    System.Console.WriteLine("Exception Message:")
    System.Console.WriteLine(e.Message)
    System.Console.WriteLine("Exception Stack Trace:")
    System.Console.WriteLine(e.StackTrace)

let private failedToParseFileError (file:string) parseException =
    printfn "%A" file
    printException parseException

/// the default only prints something if FSharpLint found a lint in a file
let private defaultProgress = function
    | FSharpLint.Application.RunLint.Starting(file)
    | FSharpLint.Application.RunLint.ReachedEnd(file) -> ()
    | FSharpLint.Application.RunLint.Failed(file, parseException) ->
        failedToParseFileError file parseException

let private defaultErrorReceived (error:ErrorHandling.Error) =
    let output = error.Info + System.Environment.NewLine + ErrorHandling.getCompleteErrorText error.Range error.Input 
    System.Console.WriteLine(output)

let defaultLintOptions =
    {
        FinishEarly = System.Func<_>(defaultFinishEarly)
        Progress = System.Action<RunLint.ParserProgress>(defaultProgress)
        ErrorReceived = System.Action<ErrorHandling.Error>(defaultErrorReceived)
        FSharpCoreDirectory = None
    }

/// Runs FSharpLint on a project.
/// ## Parameters
/// 
///  - `setParams ` - Function used to manipulate the default FSharpLint value.
///  - `projectFile` - The project file of the project you want to lint
/// 
/// ## Sample usage
///
///     Target "Lint" (fun _ ->
///         FSharpLint (fun o -> { o with ErrorReceived = System.Action<ErrorHandling.Error>(customErrorFunction) }) projectFile
///     )
let FSharpLint (setParams: LintOptions->LintOptions) (projectFile: string) =
    let parameters = defaultLintOptions |> setParams
    let lintOptions: RunLint.ProjectParseInfo =
        {
            FinishEarly = parameters.FinishEarly
            ProjectFile = projectFile
            Progress = parameters.Progress
            ErrorReceived = parameters.ErrorReceived
            FSharpCoreDirectory = parameters.FSharpCoreDirectory
        }
    RunLint.parseProject lintOptions |> printfn "%A"