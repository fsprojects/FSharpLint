module FSharpLint.FAKE

open Fake
open FSharpLint.Worker

type LintOptions =
    {
        /// Callback that's called at the start and end of parsing each file (or when a file fails to be parsed).
        Progress: System.Action<Progress>

        /// Callback that's called when a lint error is detected.
        ErrorReceived: System.Action<Error>

        /// Fail the FAKE build script if one or more lint warnings are found in a project.
        FailBuildIfAnyWarnings: bool
    }

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

type FSharpLintWorker() = 
    inherit MarshalByRefObject() 

    let reportsReceived = new Collections.Concurrent.BlockingCollection<obj>()

    let cancelToken = new Threading.CancellationTokenSource()

    let taskCompletionSource = Threading.Tasks.TaskCompletionSource<bool>()

    [<DefaultValue>] val mutable Options : LintOptions

    let getWorker () = 
        let fullPath = Reflection.Assembly.GetExecutingAssembly().Location

        let directory = IO.Path.GetDirectoryName(fullPath)

        let setup = AppDomainSetup(LoaderOptimization = System.LoaderOptimization.MultiDomain, PrivateBinPath = directory, ApplicationBase = directory, DisallowBindingRedirects = true)

        let evidence = AppDomain.CurrentDomain.Evidence

        let appDomain = AppDomain.CreateDomain("Cross Lang Domain", evidence, setup)

        appDomain.CreateInstanceAndUnwrap("FSharpLint.CrossDomain", "FSharpLint.CrossDomain.FSharpLintWorker") :?> FSharpLint.Worker.IFSharpLintWorker

    member this.RunLint projectFile (options:LintOptions) =
        this.Options <- options

        let worker = getWorker()

        ErrorReceivedEventHandler(this.ReportError) |> worker.add_ErrorReceived 

        ReportProgressEventHandler(this.ReportProgress) |> worker.add_ReportProgress

        let task = new Threading.Tasks.Task(Action(this.ReportResults))

        task.Start()

        let result = worker.RunLint(projectFile)

        cancelToken.Cancel(false)

        task.Wait()

        this.ClearAnyReportsReceived()

        result

    [<Runtime.Remoting.Messaging.OneWay>]
    member this.ReportError(error:Error) =
        reportsReceived.Add(error)

    [<Runtime.Remoting.Messaging.OneWay>]
    member this.ReportProgress(progress:Progress) =
        reportsReceived.Add(progress)

    interface IDisposable with
        member this.Dispose() =
            this.Dispose(true)
            GC.SuppressFinalize(this)

    member private this.ClearAnyReportsReceived() =
        let rec clearReportsReceived () =
            if reportsReceived.Count > 0 then
                reportsReceived.Take() |> this.PassReportToOptionsCallback
                clearReportsReceived()

        clearReportsReceived()

    member private this.PassReportToOptionsCallback = function
        | :? Error as error -> this.Options.ErrorReceived.Invoke(error)
        | :? Progress as progress -> this.Options.Progress.Invoke(progress)
        | _ -> ()

    member private this.ReportResults() =
        while not cancelToken.IsCancellationRequested do
            try
                reportsReceived.Take(cancelToken.Token) 
                    |> this.PassReportToOptionsCallback
            with _ -> ()

    member private this.Dispose(disposing) =
        if disposing then
            reportsReceived.Dispose()
            cancelToken.Dispose()

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

    use worker = new FSharpLintWorker()
    let result = worker.RunLint projectFile options

    if result.IsSuccess && parameters.FailBuildIfAnyWarnings && !numberOfWarnings > 0 then
        failwithf "Linted %s and failed the build as warnings were found. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
    else if result.IsSuccess then
        tracefn "Successfully linted %s. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
    else
        sprintf "Failed to lint %s. Failed with: %s" projectFile result.Message |> traceError

    traceEndTask "FSharpLint" projectFile