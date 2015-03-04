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

type FSharpLintTask() = 
    inherit Microsoft.Build.Utilities.Task()

    [<Microsoft.Build.Framework.Required>]
    member val Project = "" with get, set

    member val TreatWarningsAsErrors = false with get, set

    override this.Execute() = 
        let fullPath = System.Reflection.Assembly.GetAssembly(this.GetType()).Location;

        let directory = System.IO.Path.GetDirectoryName(fullPath)

        let setup = System.AppDomainSetup(PrivateBinPath = directory, ApplicationBase = directory)

        let evidence = System.AppDomain.CurrentDomain.Evidence

        let appDomain = System.AppDomain.CreateDomain("Lint Domain", evidence, setup)

        let worker = appDomain.CreateInstanceAndUnwrap("FSharpLint.Application", "FSharpLint.Application.RunLint+FSharpLintWorker") :?> FSharpLint.Worker.IFSharpLintWorker

        // Cannot close over `this` in the function passed to `RunLint` or it'll try to serialize `this` (which will throw an exception).
        let treatWarningsAsErrors = this.TreatWarningsAsErrors
        let logWarning:(string * string * string * string * int * int * int * int * string * obj[]) -> unit = this.Log.LogWarning
        let logError:(string * string * string * string * int * int * int * int * string * obj[]) -> unit = this.Log.LogError
        let logFailure:(string -> unit) = this.Log.LogWarning

        let errorReceived (error:FSharpLint.Worker.Error) = 
            let filename = error.Range.FileName
            let startLine = error.Range.StartLine
            let startColumn = error.Range.StartColumn + 1
            let endLine = error.Range.EndLine
            let endColumn = error.Range.EndColumn + 1

            if treatWarningsAsErrors then
                logError("", "", "", filename, startLine, startColumn, endLine, endColumn, error.FormattedError, null)
            else
                logWarning("", "", "", filename, startLine, startColumn, endLine, endColumn, error.FormattedError, null)

        let reportProgress (progress:FSharpLint.Worker.Progress) =
            if progress.State = FSharpLint.Worker.Progress.ProgressType.Failed then
                sprintf 
                    "Failed to parse file %s, Exception Message: %s \nException Stack Trace: %s" 
                    progress.Filename 
                    progress.Exception.Message 
                    progress.Exception.StackTrace
                    |> logFailure

        worker.add_ErrorReceived(FSharpLint.Worker.ErrorReceivedEventHandler(errorReceived))
        worker.add_ReportProgress(FSharpLint.Worker.ReportProgressEventHandler(reportProgress))

        let result = worker.RunLint(this.Project)

        if not result.IsSuccess then
            logFailure(result.Message)
            
        true