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
                    let getFailureReason (x:Microsoft.FSharp.Compiler.FSharpErrorInfo) =
                        sprintf "failed to parse file %s, message: %s" x.FileName x.Message

                    String.Join(", ", failures |> Array.map getFailureReason)
                | ParseFile.AbortedTypeCheck -> "Aborted type check."

            match lintProject options projectFile progress with
            | LintResult.Failure(ProjectFileCouldNotBeFound(projectPath)) -> 
                failed "ConsoleProjectFileCouldNotBeFound" [|projectPath|]
            | LintResult.Failure(MSBuildFailedToLoadProjectFile(projectPath, e)) -> 
                failed "ConsoleMSBuildFailedToLoadProjectFile" [|projectPath; e.Message|]
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