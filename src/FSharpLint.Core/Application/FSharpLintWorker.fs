namespace FSharpLint.Application

module FSharpLintWorker =

    open System
    open FSharpLint.Framework

    type Result =
        | Success
        | Failure of string

    let RunLint(projectFile, options, progress) =
        let failed resouce args = 
            let formatString = Resources.GetString resouce
            String.Format(formatString, args) |> Result.Failure

        try
            let getParseFailureReason = function
                | ParseFile.FailedToParseFile(failures) ->
                    let getFailureReason (x:Microsoft.FSharp.Compiler.SourceCodeServices.FSharpErrorInfo) =
                        sprintf "failed to parse file %s, message: %s" x.FileName x.Message

                    String.Join(", ", failures |> Array.map getFailureReason)
                | ParseFile.AbortedTypeCheck -> "Aborted type check."

            match lintProject options projectFile progress with
            | LintResult.Failure(ProjectFileCouldNotBeFound(projectPath)) -> 
                failed "ConsoleProjectFileCouldNotBeFound" [|projectPath|]
            | LintResult.Failure(MSBuildFailedToLoadProjectFile(projectPath, InvalidProjectFileMessage(message))) -> 
                failed "ConsoleMSBuildFailedToLoadProjectFile" [|projectPath; message|]
            | LintResult.Failure(FailedToLoadConfig(message)) -> 
                failed "ConsoleFailedToLoadConfig" [|message|]
            | LintResult.Failure(RunTimeConfigError) -> 
                failed "ConsoleRunTimeConfigError" [||]
            | LintResult.Failure(FailedToParseFile(failure)) -> 
                Result.Failure(
                    "Lint failed while analysing " + 
                    projectFile + 
                    ".\nFailed with: " + 
                    getParseFailureReason failure)
            | LintResult.Failure(FailedToParseFilesInProject(failures)) -> 
                Result.Failure(
                    "Lint failed while analysing " + 
                    projectFile + 
                    ".\nFailed with: " + 
                    String.Join("\n", failures |> List.map getParseFailureReason))
            | LintResult.Success(_) -> Result.Success
        with
        | e -> 
            "Lint failed while analysing " + projectFile + ".\nFailed with: " + e.Message + "\nStack trace: " + e.StackTrace
            |> Result.Failure