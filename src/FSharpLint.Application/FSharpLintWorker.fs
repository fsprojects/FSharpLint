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

module FSharpLintWorker =

    open FSharpLint.Framework
    
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
                        
    let private toWorkerError (error:LintWarning.Warning) = 
        FSharpLint.Worker.Error(Info = error.Info,
                                Range = toWorkerRange error.Range,
                                Input = error.Input,
                                FormattedError = LintWarning.getWarningWithLocation error.Range error.Input)

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

                    let parseInfo =
                        {
                            FinishEarly = Some neverFinishEarly
                            ReceivedWarning = Some receivedError
                            Configuration = None
                        }

                    let getParseFailureReason = function
                        | ParseFile.FailedToParseFile(failures) ->
                            let getFailureReason (x:Microsoft.FSharp.Compiler.FSharpErrorInfo) =
                                sprintf "failed to parse file %s, message: %s" x.FileName x.Message

                            System.String.Join(", ", failures |> Array.map getFailureReason)
                        | ParseFile.AbortedTypeCheck ->
                            "Aborted type check."

                    match lintProject parseInfo projectFile (Some receivedProgress) with
                        | LintResult.Failure(ProjectFileCouldNotBeFound(projectPath)) -> 
                            failed "ConsoleProjectFileCouldNotBeFound" [|projectPath|]
                        | LintResult.Failure(MSBuildFailedToLoadProjectFile(projectPath, e)) -> 
                            failed "ConsoleMSBuildFailedToLoadProjectFile" [|projectPath; e.Message|]
                        | LintResult.Failure(FailedToLoadConfig(message)) -> 
                            failed "ConsoleFailedToLoadConfig" [|message|]
                        | LintResult.Failure(RunTimeConfigError) -> 
                            failed "ConsoleRunTimeConfigError" [||]
                        | LintResult.Failure(FailedToParseFile(failure)) -> 
                            FSharpLint.Worker.Result.Failure(
                                "Lint failed while analysing " + 
                                projectFile + 
                                ".\nFailed with: " + 
                                getParseFailureReason failure)
                        | LintResult.Failure(FailedToParseFilesInProject(failures)) -> 
                            FSharpLint.Worker.Result.Failure(
                                "Lint failed while analysing " + 
                                projectFile + 
                                ".\nFailed with: " + 
                                System.String.Join("\n", failures |> List.map getParseFailureReason))
                        | LintResult.Success(_) -> 
                            FSharpLint.Worker.Result.Success()
                with
                    | e -> 
                        FSharpLint.Worker.Result.Failure("Lint failed while analysing " + projectFile + ".\nFailed with: " + e.Message + "\nStack trace: " + e.StackTrace)