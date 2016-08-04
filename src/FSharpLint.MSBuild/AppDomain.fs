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
   
open System
open FSharpLint.Application
open FSharpLint.Application.FSharpLintWorker

module AppDomain =

    type Warning =
        { Filename: String
          StartLine: int
          StartColumn: int 
          EndLine: int
          EndColumn: int
          Info: string }

    [<Serializable>]
    type FailureEventArgs(filename, e) =
        inherit EventArgs()

        member __.Filename: string = filename
        member __.Exception: exn = e

    type FailureEventHandler = delegate of obj * FailureEventArgs -> unit

    type LintRunner() = 
        inherit MarshalByRefObject()
        
        let failureEvent = Event<FailureEventHandler, FailureEventArgs>()

        [<CLIEvent>]
        member this.Failure = failureEvent.Publish

        member __.Lint(projectFile) =
            let warnings = ResizeArray()

            let errorReceived (error:LintWarning.Warning) = 
                warnings.Add
                    { Filename = error.Range.FileName
                      StartLine = error.Range.StartLine
                      StartColumn = error.Range.StartColumn + 1
                      EndLine = error.Range.EndLine
                      EndColumn = error.Range.EndColumn + 1
                      Info = error.Info }
        
            let options = 
                { ReceivedWarning = Some(errorReceived)
                  FinishEarly = None
                  Configuration = None }

            let progressReceived = function
                | ProjectProgress.Failed(file, e) -> 
                    failureEvent.Trigger(null, FailureEventArgs(file, e))
                | _ -> ()                

            match FSharpLintWorker.RunLint(projectFile, options, Some(progressReceived)) with
            | Success -> warnings
            | Failure(message) -> failwith message