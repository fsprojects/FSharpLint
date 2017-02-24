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
                  CancellationToken = None
                  Configuration = None }

            let progressReceived = function
                | ProjectProgress.Failed(file, e) -> 
                    failureEvent.Trigger(null, FailureEventArgs(file, e))
                | _ -> ()                

            match FSharpLintWorker.RunLint(projectFile, options, Some(progressReceived)) with
            | Success -> warnings
            | Failure(message) -> failwith message