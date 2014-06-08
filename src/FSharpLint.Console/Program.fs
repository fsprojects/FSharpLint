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

namespace FSharpLint.Console

module Program =

    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Application

    let private help () =
        System.Console.WriteLine(Resources.GetString("ConsoleHelp"))

    let private printException (e:System.Exception) =
        System.Console.WriteLine("Exception Message:")
        System.Console.WriteLine(e.Message)
        System.Console.WriteLine("Exception Stack Trace:")
        System.Console.WriteLine(e.StackTrace)

    let private failedToParseFileError (file:string) parseException =
        let formatString = Resources.GetString("ConsoleFailedToParseFile")
        System.Console.WriteLine(System.String.Format(formatString, file))
        printException parseException

    let private parserProgress = function
        | FSharpLint.Application.RunLint.Starting(_)
        | FSharpLint.Application.RunLint.ReachedEnd(_) -> ()
        | FSharpLint.Application.RunLint.Failed(file, parseException) ->
            failedToParseFileError file parseException

    let private runLint projectFile =
        let finishEarly = System.Func<_>(fun _ -> false)

        let parserProgress = System.Action<RunLint.ParserProgress>(parserProgress)

        let error = System.Action<ErrorHandling.Error>(fun error -> 
            System.Console.WriteLine(error.Info)
            System.Console.WriteLine(ErrorHandling.errorInfoLine error.Range error.Input))

        RunLint.parseProject(finishEarly, projectFile, parserProgress, error)

    let private printFailedDescription = function
        | ProjectFile.ProjectFileCouldNotBeFound(projectPath) ->
            let formatString = Resources.GetString("ConsoleProjectFileCouldNotBeFound")
            System.Console.WriteLine(System.String.Format(formatString, projectPath))

        | ProjectFile.MSBuildFailedToLoadProjectFile(projectPath, e) ->
            let formatString = Resources.GetString("ConsoleMSBuildFailedToLoadProjectFile")
            System.Console.WriteLine(System.String.Format(formatString, projectPath, e.Message))

        | ProjectFile.MSBuildFailedToLoadReferencedProjectFile(referencedProjectPath, e) ->
            let formatString = Resources.GetString("ConsoleMSBuildFailedToLoadReferencedProjectFile")
            System.Console.WriteLine(System.String.Format(formatString, referencedProjectPath, e.Message))

        | ProjectFile.UnableToFindProjectOutputPath(projectPath) ->
            let formatString = Resources.GetString("ConsoleUnableToFindProjectOutputPath")
            System.Console.WriteLine(System.String.Format(formatString, projectPath))

        | ProjectFile.UnableToFindReferencedProject(referencedProjectPath) ->
            let formatString = Resources.GetString("ConsoleUnableToFindReferencedProject")
            System.Console.WriteLine(System.String.Format(formatString, referencedProjectPath))

        | ProjectFile.UnableToFindFSharpCoreDirectory ->
            System.Console.WriteLine(Resources.GetString("ConsoleUnableToFindFSharpCoreDirectory"))

        | ProjectFile.FailedToLoadConfig(message) ->
            let formatString = Resources.GetString("ConsoleFailedToLoadConfig")
            System.Console.WriteLine(System.String.Format(formatString, message))

        | ProjectFile.RunTimeConfigError ->
            System.Console.WriteLine(Resources.GetString("ConsoleRunTimeConfigError"))

        | ProjectFile.FailedToResolveReferences ->
            System.Console.WriteLine(Resources.GetString("ConsoleFailedToResolveReferences"))
    
    [<EntryPoint>]
    let main argv = 
        if argv.Length < 2 then
            help()
        else
            match argv.[0] with
                | "-f" -> 
                    let projectFile = argv.[1]

                    if System.IO.File.Exists(projectFile) then
                        match runLint projectFile with
                            | RunLint.Success ->
                                System.Console.WriteLine(Resources.GetString("ConsoleFinished"))
                            | RunLint.Failure(error) ->
                                printFailedDescription error
                    else
                        let formatString = Resources.GetString("ConsoleCouldNotFindFile")
                        System.Console.WriteLine(System.String.Format(formatString, projectFile))
                | _ -> help()

        0