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
        |> traceError

let private failedToParseFileError (file:string) parseException =
    printfn "%A" file
    printException parseException

/// the default only prints something if FSharpLint found a lint in a file
let private defaultProgress = function
    | Starting(file)
    | ReachedEnd(file) -> ()
    | Failed(file, parseException) ->
        failedToParseFileError file parseException

let private defaultErrorReceived (error:Error) =
    error.Info + System.Environment.NewLine + error.FormattedError
        |> traceFAKE "%s"

let defaultLintOptions =
    {
        FinishEarly = System.Func<_>(defaultFinishEarly)
        Progress = System.Action<Progress>(defaultProgress)
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
    let parameters = defaultLintOptions |> setParams

    traceStartTask "FSharpLint" projectFile

    let numberOfWarnings, numberOfFiles = ref 0, ref 0

    System.AppDomain.CurrentDomain.GetAssemblies()
        |> Array.iter (fun x -> printf "%s %s %s\n" x.FullName x.CodeBase x.Location)

    printf "\n\n"

    let fullPath = System.Reflection.Assembly.GetExecutingAssembly().Location;

    let directory = System.IO.Path.GetDirectoryName(fullPath)

    let setup = System.AppDomainSetup(PrivateBinPath = directory, ApplicationBase = directory, DisallowBindingRedirects = true)

    let evidence = System.AppDomain.CurrentDomain.Evidence

    let appDomain = System.AppDomain.CreateDomain("Lint Domain", evidence, setup)

    System.AppDomain.CurrentDomain.add_AssemblyResolve(System.ResolveEventHandler(fun x args ->
        let assembly = System.Reflection.Assembly.Load(args.Name)
        if assembly <> null then
            assembly
        else
            let parts = args.Name.Split(',')
            let file = System.IO.Path.Combine(directory, parts.[0].Trim() + ".dll")

            System.Reflection.Assembly.LoadFrom(file)))
        
    let worker = appDomain.CreateInstanceAndUnwrap("FSharpLint.Application", "FSharpLint.Application.RunLint+FSharpLintWorker") :?> FSharpLint.Worker.IFSharpLintWorker

    let errorReceived error = 
        incr numberOfWarnings
        parameters.ErrorReceived.Invoke(error)

    let parserProgress progress =
        match progress with
            | ReachedEnd(_) -> incr numberOfFiles
            | _ -> ()
        parameters.Progress.Invoke(progress)

    let options = 
        {
            FSharpLint.Worker.FinishEarly = parameters.FinishEarly
            Progress = parameters.Progress
            ErrorReceived = parameters.ErrorReceived
        }

    match worker.RunLint projectFile (*options*) with
        | Success when parameters.FailBuildIfAnyWarnings && !numberOfWarnings > 0 ->
            failwithf "Linted %s and failed the build as warnings were found. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
        | Success ->
            tracefn "Successfully linted %s. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
        | Failure(error) ->
            sprintf "Failed to lint %s. Failed with: %s" projectFile error |> traceError

    traceEndTask "FSharpLint" projectFile