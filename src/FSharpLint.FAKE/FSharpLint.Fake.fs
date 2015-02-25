module FSharpLint.FAKE

open Fake
open FSharpLint.Worker

type LintOptions =
    {
        /// Function that when returns true cancels the parsing of the project, useful for cancellation tokens etc.
        FinishEarly: System.Func<bool>

        /// Callback that's called at the start and end of parsing each file (or when a file fails to be parsed).
        Progress: System.Action<Progress>

        /// Callback that's called when a lint error is detected.
        ErrorReceived: System.Action<Error>

        /// Fail the FAKE build script if one or more lint warnings are found in a project.
        FailBuildIfAnyWarnings: bool
    }

/// the default never finishes early
let private defaultFinishEarly _ = false 

let private printException (e:System.Exception) =
    "Exception Message:" + e.Message + "Exception Stack Trace:" + e.StackTrace
        |> printf "%s"

let private failedToParseFileError (file:string) parseException =
    printfn "%A" file
    printException parseException
    (*
/// the default only prints something if FSharpLint found a lint in a file
let private defaultProgress = function
    | Starting(file)
    | ReachedEnd(file) -> ()
    | Failed(file, parseException) ->
        failedToParseFileError file parseException
        *)
let private defaultErrorReceived (error:Error) =
    error.Info + System.Environment.NewLine + error.FormattedError
        |> printf "%s"

let defaultLintOptions =
    {
        FinishEarly = System.Func<_>(defaultFinishEarly)
        Progress = System.Action<Progress>(ignore)
        ErrorReceived = System.Action<Error>(defaultErrorReceived)
        FailBuildIfAnyWarnings = false
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
    //let parameters = defaultLintOptions |> setParams

    traceStartTask "FSharpLint" projectFile

    let numberOfWarnings, numberOfFiles = ref 0, ref 0

    let fullPath = System.Reflection.Assembly.GetExecutingAssembly().Location;

    let directory = System.IO.Path.GetDirectoryName(fullPath)

    let setup = System.AppDomainSetup(PrivateBinPath = directory, ApplicationBase = directory, DisallowBindingRedirects = true)

    let evidence = System.AppDomain.CurrentDomain.Evidence

    let appDomain = System.AppDomain.CreateDomain("Cross Lang Domain", evidence, setup)

    let worker = appDomain.CreateInstanceAndUnwrap("FSharpLint.CrossDomain", "FSharpLint.CrossDomain.FSharpLintWorker") :?> FSharpLint.Worker.IFSharpLintWorker
    (*
    let errorReceived error = 
        incr numberOfWarnings
        parameters.ErrorReceived.Invoke(error)

    let parserProgress progress =
        match progress with
            | ReachedEnd(_) -> incr numberOfFiles
            | _ -> ()
        parameters.Progress.Invoke(progress)*)

    let options = FSharpLint.Worker.LintOptions(FinishEarly = System.Func<_>(defaultFinishEarly), 
                                                Progress = System.Action<_>(ignore), 
                                                ErrorReceived = System.Action<_>(defaultErrorReceived))

    let result = worker.RunLint(projectFile, options)
    //    | Success when parameters.FailBuildIfAnyWarnings && !numberOfWarnings > 0 ->
     //       failwithf "Linted %s and failed the build as warnings were found. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
    if result.IsSuccess then
        printf "Successfully linted %s. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
    else
        sprintf "Failed to lint %s. Failed with: %s" projectFile result.Message |> printf "%s"

    traceEndTask "FSharpLint" projectFile