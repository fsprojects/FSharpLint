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

namespace FSharpLint.MSBuildIntegration

open System
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

type FSharpLintTask() = 
    inherit Microsoft.Build.Utilities.Task()

    [<Microsoft.Build.Framework.Required>]
    member val Project = "" with get, set

    member val TreatWarningsAsErrors = false with get, set

    override this.Execute() = 
        let directory = System.Reflection.Assembly.GetAssembly(this.GetType()).Location
                        |> System.IO.Path.GetDirectoryName

        let setup = System.AppDomainSetup(PrivateBinPath = directory, ApplicationBase = directory)

        let appDomain = System.AppDomain.CreateDomain("Lint Domain", null, setup)

        let resolveAssembly _ (args:ResolveEventArgs) =
            let assembly = 
                try 
                    match System.Reflection.Assembly.Load(args.Name) with
                        | null -> None
                        | assembly -> Some(assembly)
                with _ -> None

            match assembly with
                | Some(assembly) -> assembly
                | None -> 
                    let parts = args.Name.Split(',')
                    let file = System.IO.Path.Combine(directory, parts.[0].Trim() + ".dll")

                    System.Reflection.Assembly.LoadFrom(file)
            
        System.AppDomain.CurrentDomain.add_AssemblyResolve(System.ResolveEventHandler(resolveAssembly))
        
        let worker = appDomain.CreateInstanceAndUnwrap("FSharpLint.Application", "FSharpLint.Application.FSharpLintWorker+FSharpLintWorker") :?> FSharpLint.Worker.IFSharpLintWorker

        // Cannot close over `this` in the function passed to `RunLint` or it'll try to serialize `this` (which will throw an exception).
        let treatWarningsAsErrors = this.TreatWarningsAsErrors
        let logWarning:(string * string * string * string * int * int * int * int * string * obj[]) -> unit = this.Log.LogWarning
        let logError:(string * string * string * string * int * int * int * int * string * obj[]) -> unit = this.Log.LogError
        let logFailure:(string -> unit) = this.Log.LogWarning

        let progress (progress:FSharpLint.Worker.Progress) =
            if progress.State = FSharpLint.Worker.Progress.ProgressType.Failed then
                sprintf 
                    "Failed to parse file %s, Exception Message: %s \nException Stack Trace: %s"
                    progress.Filename
                    progress.Exception.Message
                    progress.Exception.StackTrace
                    |> logFailure

        let neverFinishEarly = fun _ -> false

        let errorReceived (error:FSharpLint.Worker.Error) = 
            let filename = error.Range.FileName
            let startLine = error.Range.StartLine
            let startColumn = error.Range.StartColumn + 1
            let endLine = error.Range.EndLine
            let endColumn = error.Range.EndColumn + 1

            if treatWarningsAsErrors then
                logError("", "", "", filename, startLine, startColumn, endLine, endColumn, error.Info, null)
            else
                logWarning("", "", "", filename, startLine, startColumn, endLine, endColumn, error.Info, null)

        //worker.add_ErrorReceived(FSharpLint.Worker.ErrorReceivedEventHandler(errorReceived))
        //worker.add_ReportProgress(FSharpLint.Worker.ReportProgressEventHandler(progress))
        
        let options = 
            { 
                Progress = System.Action<_>(progress)
                ErrorReceived = System.Action<_>(errorReceived)
                FailBuildIfAnyWarnings = treatWarningsAsErrors
            }
        
        use worker = new FSharpLintWorker()
        let result = worker.RunLint this.Project options

        if not result.IsSuccess then
            logFailure result.Message
            
        true