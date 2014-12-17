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
        System.Reflection.Assembly.Load("FSharpLint.Rules")
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

            let parseInfo = FSharpLint.Framework.Ast.parseFile checker projectOptions file input

            let visitPlainText = async {
                    let suppressMessageAttributes =
                        if input.Contains("SuppressMessage") then
                            FSharpLint.Framework.Ast.getSuppressMessageAttributesFromAst parseInfo.Ast
                        else []

                    for visitor in plainTextVisitors plugins visitorInfo do
                        visitor { Input = input; File = file; SuppressedMessages = suppressMessageAttributes }
                }

            let visitAst = async {
                    try
                        astVisitors plugins visitorInfo
                            |> FSharpLint.Framework.Ast.parse finishEarly parseInfo
                    with 
                        | e -> 
                            progress.Invoke(Failed(file, e))
                }

            progress.Invoke(Starting(file))

            [visitAst; visitPlainText]
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore

            progress.Invoke(ReachedEnd(file))

    /// Creates a project options object that is required by the compiler.
    let loadProjectOptions (projectFile:ProjectFile.ProjectFile) (checker:Microsoft.FSharp.Compiler.SourceCodeServices.FSharpChecker) = 
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
                    yield! projectFile.FSharpFiles |> List.map (fun x -> x.FileLocation)
                    for r in projectFile.References do yield "-r:" + r
                    for r in projectFile.ProjectReferences do yield "-r:" + r
                |])

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
        
    /// Parses and runs the linter on all the files in a project.
    let parseProject projectInformation = 
        let finishEarly = fun _ -> projectInformation.FinishEarly.Invoke()

        match ProjectFile.loadProjectFile projectInformation.ProjectFile projectInformation.FSharpCoreDirectory with
            | ProjectFile.Success(projectFile) -> 
                let checker = Microsoft.FSharp.Compiler.SourceCodeServices.FSharpChecker.Create()
        
                let projectOptions = loadProjectOptions projectFile checker

                try
                    let plugins = loadPlugins()

                    projectFile.FSharpFiles 
                        |> List.choose (fun x -> if x.ExcludeFromAnalysis then None else Some(x.FileLocation))
                        |> List.iter (parseFile finishEarly projectInformation.ErrorReceived projectInformation.Progress projectFile checker plugins projectOptions)

                    Success
                with 
                    | FSharpLint.Framework.Configuration.ConfigurationException(_) -> 
                        Failure(ProjectFile.RunTimeConfigError)
            | ProjectFile.Failure(error) -> 
                Failure(error)