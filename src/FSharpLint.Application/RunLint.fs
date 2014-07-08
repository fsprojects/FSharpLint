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

module RunLint =

    type Result = 
        | Success
        | Failure of ProjectFile.Error

    type ParserProgress =
        | Starting of string
        | ReachedEnd of string
        | Failed of string * FSharpLint.Framework.Ast.ParseException

        member this.Filename() =
            match this with 
                | Starting(f) 
                | ReachedEnd(f)
                | Failed(f, _) -> f

    let loadPlugins () =
        System.Reflection.Assembly.Load("FSharpLint.Rules")
            |> FSharpLint.Framework.LoadVisitors.loadPlugins

    let astVisitors (plugins:FSharpLint.Framework.LoadVisitors.VisitorPlugin list) visitorInfo =
        [ for plugin in plugins do
            match plugin.Visitor with
                | FSharpLint.Framework.LoadVisitors.Ast(visitor) -> 
                    yield visitor visitorInfo
                | FSharpLint.Framework.LoadVisitors.PlainText(_) -> ()
        ]

    let plainTextVisitors (plugins:FSharpLint.Framework.LoadVisitors.VisitorPlugin list) visitorInfo =
        [ for plugin in plugins do
            match plugin.Visitor with
                | FSharpLint.Framework.LoadVisitors.Ast(_) -> ()
                | FSharpLint.Framework.LoadVisitors.PlainText(visitor) -> 
                    yield visitor visitorInfo
        ]

    let parseFile finishEarly (errorReceived:System.Action<ErrorHandling.Error>) (progress:System.Action<ParserProgress>) (project:ProjectFile.ProjectFile) checker plugins projectOptions file =
        if not <| finishEarly() then
            let input = System.IO.File.ReadAllText(file)

            let postError range error =
                errorReceived.Invoke(
                    {
                        Info = error
                        Range = range
                        Input = input
                    })

            let visitorInfo = 
                {
                    FSharpLint.Framework.Ast.Config = project.Config
                    FSharpLint.Framework.Ast.PostError = postError
                }

            let visitPlainText = async {
                    for visitor in plainTextVisitors plugins visitorInfo do
                        visitor input file
                }

            let visitAst = async {
                    try
                        astVisitors plugins visitorInfo
                            |> FSharpLint.Framework.Ast.parse finishEarly checker projectOptions file input
                    with 
                        | :? FSharpLint.Framework.Ast.ParseException as e -> 
                            progress.Invoke(Failed(file, e))
                }

            progress.Invoke(Starting(file))

            [visitAst; visitPlainText]
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore

            progress.Invoke(ReachedEnd(file))

    let loadProjectOptions (projectFile:ProjectFile.ProjectFile) (checker:Microsoft.FSharp.Compiler.SourceCodeServices.InteractiveChecker) = 
        checker.GetProjectOptionsFromCommandLineArgs
            (projectFile.Path,
                [| 
                    yield "--simpleresolution" 
                    yield "--noframework" 
                    yield "--debug:full" 
                    yield "--define:DEBUG" 
                    yield "--optimize-" 
                    yield "--out:" + "dog.exe"
                    yield "--doc:test.xml" 
                    yield "--warn:3" 
                    yield "--fullpaths" 
                    yield "--flaterrors" 
                    yield "--target:exe" 
                    yield! projectFile.FSharpFiles
                    for r in projectFile.References do yield "-r:" + r
                    for r in projectFile.ProjectReferences do yield "-r:" + r
                |])
        
    /// <summary>Parses and runs the linter on all the files in a project.</summary>
    /// <param name="finishEarly">Function that when returns true cancels the parsing of the project, useful for cancellation tokens etc.</param>
    /// <param name="projectFile">Absolute path to the .fsproj file.</param>
    /// <param name="progress">Callback that's called at the start and end of parsing each file (or when a file fails to be parsed).</param>
    /// <param name="errorReceived">Callback that's called when a lint error is detected.</param>
    let parseProject (finishEarly: System.Func<bool>, projectFile:string, progress: System.Action<ParserProgress>, errorReceived: System.Action<ErrorHandling.Error>) = 
        let finishEarly = fun _ -> finishEarly.Invoke()

        match ProjectFile.loadProjectFile projectFile with
            | ProjectFile.Success(projectFile) -> 
                let checker = Microsoft.FSharp.Compiler.SourceCodeServices.InteractiveChecker.Create()
        
                let projectOptions = loadProjectOptions projectFile checker

                try
                    let plugins = loadPlugins()

                    projectFile.FSharpFiles 
                        |> List.iter (parseFile finishEarly errorReceived progress projectFile checker plugins projectOptions)

                    Success
                with 
                    | :? FSharpLint.Framework.Configuration.ConfigurationException -> 
                        Failure(ProjectFile.RunTimeConfigError)
            | ProjectFile.Failure(error) -> 
                Failure(error)