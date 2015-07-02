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

namespace FSharpLint.Application

module AppDomainWorker =

    open System
    open FSharpLint.Worker

    type LintOptions =
        {
            /// Callback that's called at the start and end of parsing each file (or when a file fails to be parsed).
            Progress: System.Action<Progress>

            /// Callback that's called when a lint error is detected.
            ErrorReceived: System.Action<Error>

            /// Fail the build if one or more lint warnings are found in a project.
            FailBuildIfAnyWarnings: bool
        }

    type AppDomainWorker() = 
        inherit MarshalByRefObject() 

        let reportsReceived = new Collections.Concurrent.BlockingCollection<obj>()

        let cancelToken = new Threading.CancellationTokenSource()

        let taskCompletionSource = Threading.Tasks.TaskCompletionSource<bool>()

        [<DefaultValue>] val mutable Options : LintOptions

        let getAppDomain () =
            let fullPath = Reflection.Assembly.GetExecutingAssembly().Location

            let directory = IO.Path.GetDirectoryName(fullPath)

            let setup = AppDomainSetup(LoaderOptimization = System.LoaderOptimization.MultiDomain, PrivateBinPath = directory, ApplicationBase = directory, DisallowBindingRedirects = true)

            let evidence = AppDomain.CurrentDomain.Evidence

            AppDomain.CreateDomain("Cross Lang Domain", evidence, setup)

        let getWorker (appDomain:AppDomain) = 
            appDomain.CreateInstanceAndUnwrap("FSharpLint.CrossDomain", "FSharpLint.CrossDomain.FSharpLintWorker") :?> FSharpLint.Worker.IFSharpLintWorker

        member this.RunLint projectFile (options:LintOptions) =
            this.Options <- options

            let appDomain = getAppDomain()

            try
                let worker = getWorker appDomain

                ErrorReceivedEventHandler(this.ReportError) |> worker.add_ErrorReceived 

                ReportProgressEventHandler(this.ReportProgress) |> worker.add_ReportProgress

                let task = new Threading.Tasks.Task(Action(this.ReportResults))

                task.Start()

                let result = worker.RunLint(projectFile)

                cancelToken.Cancel(false)

                task.Wait()

                this.ClearAnyReportsReceived()

                result
            finally
                AppDomain.Unload(appDomain)

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