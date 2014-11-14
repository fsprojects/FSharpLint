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

open FSharpLint.Application

type FSharpLintTask() as this = 
    inherit Microsoft.Build.Utilities.Task()

    let logError resouce ([<System.ParamArray>] args) = 
        let formatString = FSharpLint.Framework.Resources.GetString resouce
        System.String.Format(formatString, args) |> this.Log.LogWarning

    [<Microsoft.Build.Framework.Required>]
    member val Project = "" with get, set

    member val TreatWarningsAsErrors = false with get, set

    member val FSharpCoreDirectory: string = null with get, set

    override this.Execute() = 
        let finishEarly = System.Func<_>(fun _ -> false)
        let action = System.Action<_>(ignore)
        let error = System.Action<ErrorHandling.Error>(fun error -> 
            let (log:string*string*string*string*int*int*int*int*string*obj[]->unit) =
                if this.TreatWarningsAsErrors then
                    this.Log.LogError
                else
                    this.Log.LogWarning

            let range = error.Range
            log("", "", "", 
                range.FileName, 
                range.StartLine, 
                range.StartColumn + 1,
                range.EndLine,
                range.EndColumn + 1, 
                error.Info,
                null))

        try
            let parseInfo: RunLint.ProjectParseInfo =
                {
                    FinishEarly = finishEarly
                    ProjectFile = this.Project
                    Progress = action
                    ErrorReceived = error
                    FSharpCoreDirectory = 
                        if System.String.IsNullOrEmpty(this.FSharpCoreDirectory) then 
                            None 
                        else
                            Some(this.FSharpCoreDirectory) 
                }

            let result = RunLint.parseProject parseInfo

            match result with
                | RunLint.Result.Failure(ProjectFile.ProjectFileCouldNotBeFound(projectPath)) -> 
                    logError "ConsoleProjectFileCouldNotBeFound" [|projectPath|]
                | RunLint.Result.Failure(ProjectFile.MSBuildFailedToLoadProjectFile(projectPath, e)) -> 
                    logError "ConsoleMSBuildFailedToLoadProjectFile" [|projectPath; e.Message|]
                | RunLint.Result.Failure(ProjectFile.MSBuildFailedToLoadReferencedProjectFile(referencedProjectPath, e)) -> 
                    logError "ConsoleMSBuildFailedToLoadReferencedProjectFile" [|referencedProjectPath; e.Message|]
                | RunLint.Result.Failure(ProjectFile.UnableToFindProjectOutputPath(projectPath)) -> 
                    logError "ConsoleUnableToFindProjectOutputPath" [|projectPath|]
                | RunLint.Result.Failure(ProjectFile.UnableToFindReferencedProject(referencedProjectPath)) -> 
                    logError "ConsoleUnableToFindReferencedProject" [|referencedProjectPath|]
                | RunLint.Result.Failure(ProjectFile.UnableToFindFSharpCoreDirectory) -> 
                    logError "ConsoleUnableToFindFSharpCoreDirectory" [||]
                | RunLint.Result.Failure(ProjectFile.FailedToLoadConfig(message)) -> 
                    logError "ConsoleFailedToLoadConfig" [|message|]
                | RunLint.Result.Failure(ProjectFile.RunTimeConfigError) -> 
                    logError "ConsoleRunTimeConfigError" [||]
                | RunLint.Result.Failure(ProjectFile.FailedToResolveReferences) -> 
                    logError "ConsoleFailedToResolveReferences" [||]
                | RunLint.Result.Success -> ()
        with
            | e -> 
                this.Log.LogWarning("Lint failed while analysing " + this.Project + ".\nFailed with: " + e.Message + "\nStack trace: " + e.StackTrace)

        true