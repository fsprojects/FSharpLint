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

    type LintInfo =
        {
            FinishEarly: unit -> bool
            ErrorReceived: ErrorHandling.Error -> unit
            ReportLinterProgress: ParserProgress -> unit
            RulePlugins: LoadVisitors.VisitorPlugin list
            Configuration: Map<string, Configuration.Analyser>
        }

    let lintFile lintInfo (parsedFileInfo:Ast.ParseInfo) =
        if not <| lintInfo.FinishEarly() then
            let postError range error =
                {
                    ErrorHandling.Info = error
                    ErrorHandling.Range = range
                    ErrorHandling.Input = parsedFileInfo.Input
                } |> lintInfo.ErrorReceived

            let visitorInfo = 
                {
                    Ast.Config = lintInfo.Configuration
                    Ast.PostError = postError
                }

            let visitPlainText = async {
                    let suppressMessageAttributes =
                        if parsedFileInfo.Input.Contains("SuppressMessage") then
                            Ast.getSuppressMessageAttributesFromAst parsedFileInfo.Ast
                        else []

                    for visitor in plainTextVisitors lintInfo.RulePlugins visitorInfo do
                        visitor 
                            { 
                                Input = parsedFileInfo.Input
                                File = parsedFileInfo.File
                                SuppressedMessages = suppressMessageAttributes 
                            }
                }

            let visitAst = async {
                    try
                        astVisitors lintInfo.RulePlugins visitorInfo
                            |> Ast.parse lintInfo.FinishEarly parsedFileInfo
                    with 
                        | e -> 
                            Failed(parsedFileInfo.File, e) |> lintInfo.ReportLinterProgress
                }

            Starting(parsedFileInfo.File) |> lintInfo.ReportLinterProgress

            [visitAst; visitPlainText]
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore

            ReachedEnd(parsedFileInfo.File) |> lintInfo.ReportLinterProgress

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
        }

    open Microsoft.FSharp.Compiler.SourceCodeServices
        
    /// Parses and runs the linter on all the files in a project.
    let parseProject projectInformation = 
        let checker = FSharpChecker.Create()
        
        try
            let projectFileInfo = FSharpProjectFileInfo.Parse(projectInformation.ProjectFile)
            
            try
                match ProjectFile.loadConfigForProject projectInformation.ProjectFile with
                    | ProjectFile.Result.Success(config) ->
                        let projectOptions = checker.GetProjectOptionsFromProjectFile(projectInformation.ProjectFile)

                        let lintInformation =
                            {
                                Configuration = config
                                RulePlugins = loadPlugins()
                                FinishEarly = projectInformation.FinishEarly.Invoke
                                ErrorReceived = projectInformation.ErrorReceived.Invoke
                                ReportLinterProgress = projectInformation.Progress.Invoke
                            }

                        projectFileInfo.CompileFiles
                            |> Seq.map (getParseInfoForFileInProject checker projectOptions)
                            |> Seq.iter (lintFile lintInformation)
                
                        Success
                    | ProjectFile.Result.Failure(x) -> Failure(x)
            with 
                | FSharpLint.Framework.Configuration.ConfigurationException(_) -> 
                    Failure(ProjectFile.RunTimeConfigError)
        with
            | :? Microsoft.Build.Exceptions.InvalidProjectFileException as e ->
                Failure(ProjectFile.MSBuildFailedToLoadProjectFile(projectInformation.ProjectFile, e))

    let internal neverFinishEarly _ = false
        
    /// Parses and runs the linter on a single file.
    let parseFile pathToFile (errorReceived:System.Action<ErrorHandling.Error>) =
        let input = System.IO.File.ReadAllText(pathToFile)

        let lintInformation =
            {
                Configuration = FSharpLint.Framework.Configuration.loadDefaultConfiguration()
                RulePlugins = loadPlugins()
                FinishEarly = neverFinishEarly
                ErrorReceived = errorReceived.Invoke
                ReportLinterProgress = ignore
            }

        let parsedFileInformation = Ast.parseFile pathToFile input

        lintFile lintInformation parsedFileInformation
        
    /// Parses and runs the linter on a string.
    let parseInput input (errorReceived:System.Action<ErrorHandling.Error>) =
        let lintInformation =
            {
                Configuration = FSharpLint.Framework.Configuration.loadDefaultConfiguration()
                RulePlugins = loadPlugins()
                FinishEarly = neverFinishEarly
                ErrorReceived = errorReceived.Invoke
                ReportLinterProgress = ignore
            }

        let parsedFileInformation = Ast.parseInput input

        lintFile lintInformation parsedFileInformation