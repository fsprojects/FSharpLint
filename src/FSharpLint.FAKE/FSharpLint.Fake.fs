module FSharpLint.FAKE

open Fake
open FSharpLint.Worker
open FSharpLint.Application.AppDomainWorker

let private printException (e:System.Exception) =
    "Exception Message:" + e.Message + "Exception Stack Trace:" + e.StackTrace
        |> System.Console.WriteLine

let private failedToParseFileError (file:string) parseException =
    printfn "%A" file
    printException parseException

/// the default only prints something if FSharpLint found a lint in a file
let private defaultProgress (progress:Progress) = 
    match progress.State with
        | Progress.ProgressType.Failed ->
            failedToParseFileError progress.Filename progress.Exception
        | _ -> ()

let private defaultErrorReceived (error:Error) =
    error.Info + System.Environment.NewLine + error.FormattedError
        |> System.Console.WriteLine

let defaultLintOptions =
    {
        Progress = System.Action<Progress>(defaultProgress)
        ErrorReceived = System.Action<Error>(defaultErrorReceived)
        FailBuildIfAnyWarnings = false
    }

open System

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

    traceStartTask "FSharpLint" projectFile

    let numberOfWarnings, numberOfFiles = ref 0, ref 0
    
    let errorReceived error = 
        incr numberOfWarnings
        parameters.ErrorReceived.Invoke(error)

    let parserProgress (progress:Progress) =
        if progress.State = Progress.ProgressType.ReachedEnd then
            incr numberOfFiles

        parameters.Progress.Invoke(progress)

    let options = 
        { 
            Progress = System.Action<_>(parserProgress)
            ErrorReceived = System.Action<_>(errorReceived)
            FailBuildIfAnyWarnings = parameters.FailBuildIfAnyWarnings
        }

    use worker = new AppDomainWorker()
    let result = worker.RunLint projectFile options

    if result.IsSuccess && parameters.FailBuildIfAnyWarnings && !numberOfWarnings > 0 then
        failwithf "Linted %s and failed the build as warnings were found. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
    else if result.IsSuccess then
        tracefn "Successfully linted %s. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
    else
        sprintf "Failed to lint %s. Failed with: %s" projectFile result.Message |> traceError

    traceEndTask "FSharpLint" projectFile