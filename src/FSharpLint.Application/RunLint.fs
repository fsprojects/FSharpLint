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

    open FSharpLint.Framework

    type LintFailure =
        | ProjectFileCouldNotBeFound of string
        | MSBuildFailedToLoadProjectFile of string * Microsoft.Build.Exceptions.InvalidProjectFileException
        | UnableToFindProjectOutputPath of string
        | UnableToFindReferencedProject of string
        | FailedToLoadConfig of string
        | RunTimeConfigError
        | FailedToParseFile of ParseFile.ParseFileFailure
        | FailedToParseFilesInProject of ParseFile.ParseFileFailure list

    type Result<'t> = 
        | Success of 't
        | Failure of LintFailure

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

    let rulesAssembly =
        let directory =
            System.Reflection.Assembly.GetExecutingAssembly().Location
                |> System.IO.Path.GetDirectoryName

        let rulesAssembly = sprintf "%s%c%s" directory System.IO.Path.DirectorySeparatorChar "FSharpLint.Rules.dll"

        System.Reflection.Assembly.LoadFrom rulesAssembly

    module LoadConfiguration =

        open System.IO
        open FSharpLint.Framework.Configuration

        [<Literal>]
        let SettingsFileName = "Settings.FSharpLint"

        /// Gets all the parent directories of a given path - includes the original path directory too.
        let private getParentDirectories path =
            let rec getParentDirectories parentDirectories (directoryInfo:DirectoryInfo) =
                if directoryInfo = null then
                    parentDirectories
                else
                    getParentDirectories (directoryInfo::parentDirectories) directoryInfo.Parent

            DirectoryInfo(path) |> getParentDirectories []

        /// Overrides the default config with user defined config files.
        /// The configs can be in any directory between the root directory and the projects directory.
        /// The closer they are to the project directory the higher precedence they have.
        /// e.g. if the project directory is C:\User\Matt\Project then a config file found in 
        /// C:\User\ will be loaded before and overridden by a config file found in C:\User\Matt\.
        let private loadUserConfigFiles projectFilePath defaultConfig checkConfig =
            let projectFileDirectory = Path.GetDirectoryName projectFilePath
            let subdirectories = getParentDirectories projectFileDirectory |> List.map (fun x -> x.FullName)

            let rec loadAllConfigs configToOveride = function
                | path::paths ->
                    let filename = Path.Combine(path, SettingsFileName)

                    if File.Exists(filename) then
                        try
                            match overrideConfiguration configToOveride filename |> checkConfig with
                                | Success(config) -> loadAllConfigs config paths
                                | failure -> failure
                        with
                            | ConfigurationException(message) ->
                                Failure(FailedToLoadConfig (sprintf "Failed to load config file %s: %s" filename message))
                            | :? System.Xml.XmlException as e ->
                                Failure(FailedToLoadConfig (sprintf "Failed to load config file %s: %s" filename e.Message))
                    else
                        loadAllConfigs configToOveride paths
                | [] -> 
                    Success(configToOveride)

            loadAllConfigs defaultConfig subdirectories

        let loadConfigForProject projectFilePath =
            let configCheckers = 
                rulesAssembly
                    |> FSharpLint.Framework.LoadVisitors.loadConfigCheckers

            let checkConfig config =
                let configFailures = configCheckers 
                                        |> (FSharpLint.Framework.LoadVisitors.checkConfigsForFailures config)

                if List.isEmpty configFailures then
                    config |> Success
                else
                    Failure(FailedToLoadConfig(List.head configFailures))

            let config = 
                try
                    loadDefaultConfiguration() |> checkConfig
                with
                    | ConfigurationException(message) ->
                        Failure(FailedToLoadConfig ("Failed to load default config: " + message))

            match config with
                | Success(config) -> loadUserConfigFiles projectFilePath config checkConfig
                | x -> x
                    
    module LoadPlugins =

        /// Loads visitors implementing lint rules.
        let loadPlugins () =
            rulesAssembly
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

    type LintInfo =
        {
            FinishEarly: unit -> bool
            ErrorReceived: LintWarning.Warning -> unit
            ReportLinterProgress: ParserProgress -> unit
            RulePlugins: LoadVisitors.VisitorPlugin list
            Configuration: Configuration.Configuration
        }

    let lintFile lintInfo (parsedFileInfo:Ast.FileParseInfo) =
        if not <| lintInfo.FinishEarly() then
            let postError range error =
                {
                    LintWarning.Info = error
                    LintWarning.Range = range
                    LintWarning.Input = parsedFileInfo.PlainText
                } |> lintInfo.ErrorReceived

            let visitorInfo = 
                {
                    Ast.Config = lintInfo.Configuration
                    Ast.PostError = postError
                }

            let visitPlainText = async {
                    let suppressMessageAttributes =
                        if parsedFileInfo.PlainText.Contains("SuppressMessage") then
                            Ast.getSuppressMessageAttributesFromAst parsedFileInfo.Ast
                        else []

                    for visitor in LoadPlugins.plainTextVisitors lintInfo.RulePlugins visitorInfo do
                        visitor 
                            { 
                                Input = parsedFileInfo.PlainText
                                File = parsedFileInfo.File
                                SuppressedMessages = suppressMessageAttributes 
                            }
                }

            let visitAst = async {
                    try
                        LoadPlugins.astVisitors lintInfo.RulePlugins visitorInfo
                            |> Ast.lintFile lintInfo.FinishEarly parsedFileInfo
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
            ErrorReceived: System.Action<LintWarning.Warning>
        }

    open Microsoft.FSharp.Compiler.SourceCodeServices

    let getFilesInProject projectFilePath =
        try
            Success(FSharpProjectFileInfo.Parse(projectFilePath).CompileFiles)
        with
            | :? Microsoft.Build.Exceptions.InvalidProjectFileException as e ->
                Failure(MSBuildFailedToLoadProjectFile(projectFilePath, e))

    let loadConfigurationFilesForProject projectFilePath =
        try
            match LoadConfiguration.loadConfigForProject projectFilePath with
                | Success(config) -> Success(config)
                | Failure(x) -> Failure(x)
        with 
            | FSharpLint.Framework.Configuration.ConfigurationException(_) -> 
                Failure(RunTimeConfigError)
                
    let internal neverFinishEarly _ = false

    let private getFailedFiles = function 
        | ParseFile.Failed(failure) -> Some(failure) 
        | _ -> None

    let private getParsedFiles = function 
        | ParseFile.Success(file) -> Some(file) 
        | _ -> None
        
    /// Parses and runs the linter on all the files in a project.
    let parseProject projectInformation = 
        let filesInProject = getFilesInProject projectInformation.ProjectFile

        let parseFilesInProject config files =
            let lintInformation =
                {
                    Configuration = config
                    RulePlugins = LoadPlugins.loadPlugins()
                    FinishEarly = projectInformation.FinishEarly.Invoke
                    ErrorReceived = projectInformation.ErrorReceived.Invoke
                    ReportLinterProgress = projectInformation.Progress.Invoke
                }

            let isIgnoredFile = 
                (Configuration.IgnoreFiles.shouldFileBeIgnored config.IgnoreFiles.Files) >> not

            let parsedFiles =
                files
                    |> List.filter isIgnoredFile
                    |> List.map (fun file -> ParseFile.parseFile file config)
                            
            let failedFiles = parsedFiles |> List.choose getFailedFiles

            if List.isEmpty failedFiles then
                parsedFiles
                    |> List.choose getParsedFiles
                    |> List.iter (lintFile lintInformation)
                        
                Success()
            else
                Failure(FailedToParseFilesInProject(failedFiles))

        let loadConfigAndParseFilesInProject files =
            let config = loadConfigurationFilesForProject projectInformation.ProjectFile

            match config with
                | Success(config) -> parseFilesInProject config files
                | Failure(x) -> Failure(x)

        match filesInProject with
            | Success(files) -> loadConfigAndParseFilesInProject files
            | Failure(x) -> Failure(x)

    let private parse parser (errorReceived:System.Action<_>) =
        let lintInformation =
            {
                Configuration = FSharpLint.Framework.Configuration.loadDefaultConfiguration()
                RulePlugins = LoadPlugins.loadPlugins()
                FinishEarly = neverFinishEarly
                ErrorReceived = errorReceived.Invoke
                ReportLinterProgress = ignore
            }

        match parser lintInformation.Configuration with
            | ParseFile.Success(parseFileInformation) -> 
                lintFile lintInformation parseFileInformation
                Success()
            | ParseFile.Failed(failure) -> 
                Failure(FailedToParseFile(failure))
        
    /// Parses and runs the linter on a single file.
    let parseFile pathToFile = parse (ParseFile.parseFile pathToFile)
        
    /// Parses and runs the linter on a string.
    let parseInput input = parse (ParseFile.parseSource input)