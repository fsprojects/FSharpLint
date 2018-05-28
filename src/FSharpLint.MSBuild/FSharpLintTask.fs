namespace FSharpLint.MSBuild

open System
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open FSharpLint.Application
open FSharpLint.Application.FSharpLintWorker

type private Warning =
    { Filename: String
      StartLine: int
      StartColumn: int 
      EndLine: int
      EndColumn: int
      Info: string }

[<Serializable>]
type FSharpLintTask() = 
    inherit Task()

    [<Required>]
    member val Project = "" with get, set

    member val TreatWarningsAsErrors = false with get, set

    override this.Execute() = 
        try
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
                    this.Log.LogErrorFromException(e, showStackTrace = true, showDetail = true, file = file)
                | _ -> ()                

            let warnings =
                match FSharpLintWorker.RunLint(this.Project, options, Some(progressReceived)) with
                | Success -> warnings
                | Failure(message) -> failwith message

            for warning in warnings do
                if this.TreatWarningsAsErrors then
                    this.Log.LogError("", "", "", 
                                      warning.Filename, 
                                      warning.StartLine, warning.StartColumn, 
                                      warning.EndLine, warning.EndColumn, warning.Info)
                else
                    this.Log.LogWarning("", "", "", 
                                        warning.Filename, 
                                        warning.StartLine, warning.StartColumn, 
                                        warning.EndLine, warning.EndColumn, warning.Info)

            (not this.TreatWarningsAsErrors) || (Seq.isEmpty warnings)
        with e -> 
            this.Log.LogErrorFromException(e, showStackTrace = true, showDetail = true, file = this.Project)
            false
