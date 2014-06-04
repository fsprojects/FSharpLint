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

    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Application.ErrorHandling

    let private help () =
        System.Console.WriteLine(FSharpLint.Framework.Resources.GetString("ConsoleHelp"))

    let private printException (e:System.Exception) =
        System.Console.WriteLine("Exception Message:")
        System.Console.WriteLine(e.Message)
        System.Console.WriteLine("Exception Stack Trace:")
        System.Console.WriteLine(e.StackTrace)

    let private configurationError file configurationException =
        System.Console.WriteLine("Failed to load configuration file for project: " + file)
        printException configurationException

    let private projectFileError file projectFileException =
        System.Console.WriteLine("Failed to load project file: " + file)
        printException projectFileException

    let private failedToParseFileError file parseException =
        System.Console.WriteLine("Failed to parse file: " + file)
        printException parseException

    let private parserProgress = function
        | FSharpLint.Application.ProjectFile.Starting(_)
        | FSharpLint.Application.ProjectFile.ReachedEnd(_) -> ()
        | FSharpLint.Application.ProjectFile.Failed(file, parseException) ->
            failedToParseFileError file parseException
        | FSharpLint.Application.ProjectFile.FailedToLoadProjectFile(file, projectFileException) ->
            let e = 
                match projectFileException with 
                    | FSharpLint.Application.ProjectFile.FileNotFound(e) -> e :> System.Exception
                    | FSharpLint.Application.ProjectFile.InvalidFile(e) -> e :> System.Exception
            projectFileError file e
        | FSharpLint.Application.ProjectFile.FailedToLoadConfigurationFile(file, configurationException) -> 
            configurationError file configurationException

    let private runLint projectFile =
        let finishEarly = System.Func<_>(fun _ -> false)

        let parserProgress = System.Action<FSharpLint.Application.ProjectFile.ParserProgress>(parserProgress)

        let error = System.Action<Error>(fun error -> 
            System.Console.WriteLine(error.Info)
            System.Console.WriteLine(errorInfoLine error.Range error.Input))

        FSharpLint.Application.ProjectFile.parseProject(finishEarly, projectFile, parserProgress, error)
            |> ignore
    
    [<EntryPoint>]
    let main argv = 
        if argv.Length < 2 then
            help()
        else
            match argv.[0] with
                | "-f" -> 
                    let projectFile = argv.[1]

                    if System.IO.File.Exists(projectFile) then
                        runLint projectFile

                        System.Console.WriteLine(FSharpLint.Framework.Resources.GetString("ConsoleFinished"))
                    else
                        let formatString = FSharpLint.Framework.Resources.GetString("ConsoleCouldNotFindFile")
                        System.Console.WriteLine(System.String.Format(formatString, projectFile))
                | _ -> help()

        0