// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.MSBuild

open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open FSharpLint.Application
open FSharpLint.Application.FSharpLintWorker

type FSharpLintTask() = 
    inherit Task()

    [<Required>]
    member val Project = "" with get, set

    member val TreatWarningsAsErrors = false with get, set

    override this.Execute() = 
        let treatWarningsAsErrors = this.TreatWarningsAsErrors
        let logWarning:(string * string * string * string * int * int * int * int * string * obj[]) -> unit = this.Log.LogWarning
        let logError:(string * string * string * string * int * int * int * int * string * obj[]) -> unit = this.Log.LogError
        let logFailure:(string -> unit) = this.Log.LogWarning
        
        let progress (progress:ProjectProgress) =
            match progress with
            | Failed(filename, e) ->
                sprintf 
                    "Failed to parse file %s, Exception Message: %s \nException Stack Trace: %s"
                    filename
                    e.Message
                    e.StackTrace
                    |> logFailure
            | _ -> ()
        
        let errorReceived (error:LintWarning.Warning) = 
            let filename = error.Range.FileName
            let startLine = error.Range.StartLine
            let startColumn = error.Range.StartColumn + 1
            let endLine = error.Range.EndLine
            let endColumn = error.Range.EndColumn + 1

            if treatWarningsAsErrors then
                logError("", "", "", filename, startLine, startColumn, endLine, endColumn, error.Info, null)
            else
                logWarning("", "", "", filename, startLine, startColumn, endLine, endColumn, error.Info, null)
        
        let options = 
            { ReceivedWarning = Some(errorReceived)
              FinishEarly = None
              Configuration = None }
        
        match FSharpLintWorker.RunLint(this.Project, options, Some(progress)) with
        | Success -> ()
        | Failure(message) -> logFailure message

        true