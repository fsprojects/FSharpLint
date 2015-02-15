module FSharpLint.FAKE

open FSharpLint.Application
open FSharpLint.Framework
open Fake

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
    | FSharpLint.Application.RunLint.Starting(file)
    | FSharpLint.Application.RunLint.ReachedEnd(file) -> ()
    | FSharpLint.Application.RunLint.Failed(file, parseException) ->
        failedToParseFileError file parseException

let private defaultErrorReceived (error:ErrorHandling.Error) =
    error.Info + System.Environment.NewLine + ErrorHandling.getCompleteErrorText error.Range error.Input 
        |> traceFAKE "%s"

let defaultLintOptions =
    {
        FinishEarly = System.Func<_>(defaultFinishEarly)
        Progress = System.Action<RunLint.ParserProgress>(defaultProgress)
        ErrorReceived = System.Action<ErrorHandling.Error>(defaultErrorReceived)
        FSharpCoreDirectory = None
        FailBuildIfAnyWarnings = false
    }

let private getErrorDescription = function
    | ProjectFile.ProjectFileCouldNotBeFound(projectPath) ->
        let formatString = Resources.GetString("ConsoleProjectFileCouldNotBeFound")
        System.String.Format(formatString, projectPath)

    | ProjectFile.MSBuildFailedToLoadProjectFile(projectPath, e) ->
        let formatString = Resources.GetString("ConsoleMSBuildFailedToLoadProjectFile")
        System.String.Format(formatString, projectPath, e.Message)

    | ProjectFile.UnableToFindProjectOutputPath(projectPath) ->
        let formatString = Resources.GetString("ConsoleUnableToFindProjectOutputPath")
        System.String.Format(formatString, projectPath)

    | ProjectFile.UnableToFindReferencedProject(referencedProjectPath) ->
        let formatString = Resources.GetString("ConsoleUnableToFindReferencedProject")
        System.String.Format(formatString, referencedProjectPath)

    | ProjectFile.FailedToLoadConfig(message) ->
        let formatString = Resources.GetString("ConsoleFailedToLoadConfig")
        System.String.Format(formatString, message)

    | ProjectFile.RunTimeConfigError -> Resources.GetString("ConsoleRunTimeConfigError")

    | ProjectFile.FailedToResolveReferences -> Resources.GetString("ConsoleFailedToResolveReferences")

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

    let parserProgress progress =
        match progress with
            | RunLint.ParserProgress.ReachedEnd(_) -> incr numberOfFiles
            | _ -> ()
        parameters.Progress.Invoke(progress)

    let lintOptions: RunLint.ProjectParseInfo =
        {
            FinishEarly = parameters.FinishEarly
            ProjectFile = projectFile
            Progress = System.Action<_>(parserProgress)
            ErrorReceived = System.Action<_>(errorReceived)
            FSharpCoreDirectory = parameters.FSharpCoreDirectory
        }

    match RunLint.parseProject lintOptions with
        | RunLint.Result.Success when parameters.FailBuildIfAnyWarnings && !numberOfWarnings > 0 -> 
            failwithf "Linted %s and failed the build as warnings were found. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
        | RunLint.Result.Success -> 
            tracefn "Successfully linted %s. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
        | RunLint.Result.Failure(error) -> 
            sprintf "Failed to lint %s. Reason: \n%s" projectFile (getErrorDescription error)
                |> traceError

    traceEndTask "FSharpLint" projectFile