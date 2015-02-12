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

/// Runs the lint on an entire project using a .fsproj file.
module RunLint =

    type Result = 
        | Success
        | Failure of ProjectFile.Error

    /// Provides information on what the linter is currently doing.
    type ParserProgress =
        /// Started parsing a file.
        | Starting of string

        /// Finished parsing a file.
        | ReachedEnd of string

        /// Failed to parse a file.
        | Failed of string * System.Exception

        member this.Filename() =
            match this with 
                | Starting(f) 
                | ReachedEnd(f)
                | Failed(f, _) -> f

    /// Loads visitors implementing lint rules.
    let loadPlugins () =
        ProjectFile.loadRulesAssembly()
            |> FSharpLint.Framework.LoadVisitors.loadPlugins

    /// Extracts a list of ast visitors from a general list of visitors.
    let astVisitors (plugins:FSharpLint.Framework.LoadVisitors.VisitorPlugin list) visitorInfo =
        [ for plugin in plugins do
            match plugin.Visitor with
                | FSharpLint.Framework.LoadVisitors.Ast(visitor) -> 
                    yield visitor visitorInfo
                | FSharpLint.Framework.LoadVisitors.PlainText(_) -> ()
        ]
        
    /// Extracts a list of plain text visitors from a general list of visitors.
    let plainTextVisitors (plugins:FSharpLint.Framework.LoadVisitors.VisitorPlugin list) visitorInfo =
        [ for plugin in plugins do
            match plugin.Visitor with
                | FSharpLint.Framework.LoadVisitors.Ast(_) -> ()
                | FSharpLint.Framework.LoadVisitors.PlainText(visitor) -> 
                    yield visitor visitorInfo
        ]

    open FSharpLint.Framework

    let lintFile finishEarly (errorReceived:System.Action<ErrorHandling.Error>) (progress:System.Action<ParserProgress>) plugins config (parseInfo:Ast.ParseInfo) =
        if not <| finishEarly() then
            let postError range error =
                errorReceived.Invoke(
                    {
                        Info = error
                        Range = range
                        Input = parseInfo.Input
                    })

            let visitorInfo = 
                {
                    Ast.Config = config
                    Ast.PostError = postError
                }

            let visitPlainText = async {
                    let suppressMessageAttributes =
                        if parseInfo.Input.Contains("SuppressMessage") then
                            Ast.getSuppressMessageAttributesFromAst parseInfo.Ast
                        else []

                    for visitor in plainTextVisitors plugins visitorInfo do
                        visitor { Input = parseInfo.Input; File = parseInfo.File; SuppressedMessages = suppressMessageAttributes }
                }

            let visitAst = async {
                    try
                        astVisitors plugins visitorInfo
                            |> Ast.parse finishEarly parseInfo
                    with 
                        | e -> 
                            progress.Invoke(Failed(parseInfo.File, e))
                }

            progress.Invoke(Starting(parseInfo.File))

            [visitAst; visitPlainText]
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore

            progress.Invoke(ReachedEnd(parseInfo.File))

    let getParseInfoForFileInProject checker projectOptions file =
        let input = System.IO.File.ReadAllText(file)

        Ast.parseFileInProject checker projectOptions file input

    /// Provides information for controlling the parse of a project.
    type ProjectParseInfo =
        {
            /// Function that when returns true cancels the parsing of the project, useful for cancellation tokens etc.
            FinishEarly: System.Func<bool>

            /// Absolute path to the .fsproj file.
            ProjectFile: string

            /// Callback that's called at the start and end of parsing each file (or when a file fails to be parsed).
            Progress: System.Action<ParserProgress>

            /// Callback that's called when a lint error is detected.
            ErrorReceived: System.Action<ErrorHandling.Error>

            /// Optionally force the lint to lookup FSharp.Core.dll from this directory.
            FSharpCoreDirectory: string option
        }

    open Microsoft.FSharp.Compiler.SourceCodeServices
        
    /// Parses and runs the linter on all the files in a project.
    let parseProject projectInformation = 
        let finishEarly = fun _ -> projectInformation.FinishEarly.Invoke()

        let checker = FSharpChecker.Create()
        
        let projectOptions = checker.GetProjectOptionsFromProjectFile(projectInformation.ProjectFile)

        let projectFileInfo = FSharpProjectFileInfo.Parse(projectInformation.ProjectFile)
            
        try
            let plugins = loadPlugins()

            match ProjectFile.loadConfigForProject projectInformation.ProjectFile with
                | ProjectFile.Result.Success(config) ->
                    projectFileInfo.CompileFiles
                        |> Seq.map (getParseInfoForFileInProject checker projectOptions)
                        |> Seq.iter (lintFile finishEarly projectInformation.ErrorReceived projectInformation.Progress plugins config)
                
                    Success
                | ProjectFile.Result.Failure(x) -> Failure(x)
        with 
            | FSharpLint.Framework.Configuration.ConfigurationException(_) -> 
                Failure(ProjectFile.RunTimeConfigError)

    let private neverFinishEarly _ = false
    let private ignoreProgress = System.Action<_>(ignore) 
        
    /// Parses and runs the linter on a single file.
    let parseFile pathToFile errorReceived =
        let input = System.IO.File.ReadAllText(pathToFile)
        let checker = FSharpChecker.Create()
        let plugins = loadPlugins()
        let config = FSharpLint.Framework.Configuration.loadDefaultConfiguration()

        Ast.parseFile pathToFile input 
            |> lintFile neverFinishEarly errorReceived ignoreProgress plugins config
        
    /// Parses and runs the linter on a string.
    let parseInput input errorReceived =
        let checker = FSharpChecker.Create()
        let plugins = loadPlugins()
        let config = FSharpLint.Framework.Configuration.loadDefaultConfiguration()

        Ast.parseInput input 
            |> lintFile neverFinishEarly errorReceived ignoreProgress plugins config