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

open RunLint

module FSharpLintWorker =
    
    let private toWorkerProgress = function
        | Starting(f) -> FSharpLint.Worker.Progress.Starting(f)
        | ReachedEnd(f) -> FSharpLint.Worker.Progress.ReachedEnd(f)
        | Failed(f, e) -> FSharpLint.Worker.Progress.Failed(f, e)

    let private toWorkerRange (range:Microsoft.FSharp.Compiler.Range.range) =
        FSharpLint.Worker.Range(StartLine = range.StartLine,
                                StartColumn = range.StartColumn,
                                EndLine = range.EndLine,
                                EndColumn = range.EndColumn,
                                FileName = range.FileName)
                        
    let private toWorkerError (error:ErrorHandling.Error) = 
        FSharpLint.Worker.Error(Info = error.Info,
                                Range = toWorkerRange error.Range,
                                Input = error.Input,
                                FormattedError = ErrorHandling.getCompleteErrorText error.Range error.Input)

    type FSharpLintWorker() = 
        inherit System.MarshalByRefObject()

        let errorReceivedEvent = DelegateEvent<FSharpLint.Worker.ErrorReceivedEventHandler>()

        let receivedError error = errorReceivedEvent.Trigger [| toWorkerError error |]

        let reportProgressEvent = DelegateEvent<FSharpLint.Worker.ReportProgressEventHandler>()

        let receivedProgress progress = reportProgressEvent.Trigger [| toWorkerProgress progress |]

        interface FSharpLint.Worker.IFSharpLintWorker with

            [<CLIEvent>]
            member this.ErrorReceived = errorReceivedEvent.Publish

            [<CLIEvent>]
            member this.ReportProgress = reportProgressEvent.Publish

            member this.RunLint(projectFile) =
                let failed resouce args = 
                    let formatString = FSharpLint.Framework.Resources.GetString resouce
                    System.String.Format(formatString, args) |> FSharpLint.Worker.Result.Failure

                try
                    let neverFinishEarly _ = false

                    System.AppDomain.CurrentDomain.GetAssemblies()
                        |> Seq.iter (fun x -> printf "%s\n" x.FullName)

                    let parseInfo =
                        {
                            FinishEarly = System.Func<_>(neverFinishEarly)
                            ProjectFile = projectFile
                            Progress = System.Action<_>(receivedProgress)
                            ErrorReceived = System.Action<_>(receivedError)
                        }

                    match parseProject parseInfo with
                        | Result.Failure(ProjectFile.ProjectFileCouldNotBeFound(projectPath)) -> 
                            failed "ConsoleProjectFileCouldNotBeFound" [|projectPath|]
                        | Result.Failure(ProjectFile.MSBuildFailedToLoadProjectFile(projectPath, e)) -> 
                            failed "ConsoleMSBuildFailedToLoadProjectFile" [|projectPath; e.Message|]
                        | Result.Failure(ProjectFile.UnableToFindProjectOutputPath(projectPath)) -> 
                            failed "ConsoleUnableToFindProjectOutputPath" [|projectPath|]
                        | Result.Failure(ProjectFile.UnableToFindReferencedProject(referencedProjectPath)) -> 
                            failed "ConsoleUnableToFindReferencedProject" [|referencedProjectPath|]
                        | Result.Failure(ProjectFile.FailedToLoadConfig(message)) -> 
                            failed "ConsoleFailedToLoadConfig" [|message|]
                        | Result.Failure(ProjectFile.RunTimeConfigError) -> 
                            failed "ConsoleRunTimeConfigError" [||]
                        | Result.Failure(ProjectFile.FailedToResolveReferences) -> 
                            failed "ConsoleFailedToResolveReferences" [||]
                        | Result.Success -> 
                            FSharpLint.Worker.Result.Success()
                with
                    | FSharpLint.Framework.Ast.ParseException({ File = file; Errors = errors }) ->
                        FSharpLint.Worker.Result.Failure(
                            "Lint failed while analysing " + 
                            projectFile + 
                            ".\nFailed with: " + 
                            System.String.Join("\n", errors))
                    | e -> 
                        FSharpLint.Worker.Result.Failure("Lint failed while analysing " + projectFile + ".\nFailed with: " + e.Message + "\nStack trace: " + e.StackTrace)