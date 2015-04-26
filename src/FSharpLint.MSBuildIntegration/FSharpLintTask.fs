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
        let directory = System.Reflection.Assembly.GetAssembly(this.GetType()).Location
                        |> System.IO.Path.GetDirectoryName

        let setup = System.AppDomainSetup(PrivateBinPath = directory, ApplicationBase = directory)

        System.IO.Directory.GetFiles(directory) 
            |> String.concat System.Environment.NewLine
            |> fun x -> directory + System.Environment.NewLine + x
            |> this.Log.LogWarning

        let appDomain = System.AppDomain.CreateDomain("Lint Domain", null, setup)

        let resolveAssembly _ (args:ResolveEventArgs) =
            this.Log.LogWarning(sprintf "Resolving assembly %s" args.Name)

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
                    this.Log.LogWarning(sprintf "Trying to resolve %s" file)

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

        worker.add_ErrorReceived(FSharpLint.Worker.ErrorReceivedEventHandler(errorReceived))
        worker.add_ReportProgress(FSharpLint.Worker.ReportProgressEventHandler(progress))

        let result = worker.RunLint this.Project

        if not result.IsSuccess then
            logFailure result.Message
            
        true