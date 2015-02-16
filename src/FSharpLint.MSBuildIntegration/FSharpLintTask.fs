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

        System.AppDomain.CurrentDomain.add_AssemblyResolve(System.ResolveEventHandler(fun x args ->
            let assembly = System.Reflection.Assembly.Load(args.Name)
            if assembly <> null then
                assembly
            else
                let parts = args.Name.Split(',')
                let file = System.IO.Path.Combine(directory, parts.[0].Trim() + ".dll")

                System.Reflection.Assembly.LoadFrom(file)))
        
        let worker = appDomain.CreateInstanceAndUnwrap("FSharpLint.Application", "FSharpLint.Application.RunLint+FSharpLintWorker") :?> FSharpLint.Worker.IFSharpLintWorker

        // Cannot close over `this` in the function passed to `RunLint` or it'll try to serialize `this` (which will throw an exception).
        let treatWarningsAsErrors = this.TreatWarningsAsErrors
        let logWarning:(string * string * string * string * int * int * int * int * string * obj[]) -> unit = this.Log.LogWarning
        let logError:(string * string * string * string * int * int * int * int * string * obj[]) -> unit = this.Log.LogError
        let logFailure:(string -> unit) = this.Log.LogWarning

        let progress = function
            | FSharpLint.Worker.Starting(_)
            | FSharpLint.Worker.ReachedEnd(_) -> ()
            | FSharpLint.Worker.Failed(filename, e) ->
                logFailure(sprintf "Failed to parse file %s, Exception Message: %s \nException Stack Trace: %s" filename e.Message e.StackTrace)

        let neverFinishEarly = fun _ -> false

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

        let options = 
            {
                FSharpLint.Worker.LintOptions.FinishEarly = Func<_>(neverFinishEarly)
                FSharpLint.Worker.LintOptions.Progress = System.Action<_>(progress)
                FSharpLint.Worker.LintOptions.ErrorReceived = System.Action<_>(errorReceived)
            }

        match worker.RunLint this.Project options with
            | FSharpLint.Worker.Success -> ()
            | FSharpLint.Worker.Failure(error) ->
                logFailure(error)
            
        true