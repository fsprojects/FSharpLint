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

/// Provides an API to manage/load FSharpLint configuration files.
/// <see cref="FSharpLint.Framework.Configuration" /> for more information on
/// the default configuration and overriding configurations.
module ConfigurationManagement =

    open System.IO
    open FSharpLint.Framework.Configuration
    
    /// Reason for the linter failing.
    type ConfigFailure =
        /// Failed to load a FSharpLint configuration file.
        | FailedToLoadConfig of string

        /// Failed to analyse a loaded FSharpLint configuration at runtime e.g. invalid hint.
        | RunTimeConfigError
        
    [<RequireQualifiedAccess>]
    type ConfigurationResult = 
        | Success of Configuration
        | Failure of ConfigFailure

    /// Load a FSharpLint configuration file from the contents (string) of the file.
    let loadConfigurationFile configurationFileText =
        configuration configurationFileText

    /// Overrides a given FSharpLint configuration file with another.
    let overrideConfigurationFile configurationToOverride configurationToOverrideWith =
        overrideConfiguration configurationToOverride configurationToOverrideWith

    /// Overrides the default FSharpLint configuration.
    /// The default FSharpLint configuration contains all required elements, so
    /// by overriding it any missing required elements will be added to the returned configuration.
    /// If you're loading your own configuration you should make sure that it overrides the default 
    /// configuration/overrides a configuration that has overriden the default configuration.
    let overrideDefaultConfiguration configurationToOverrideDefault =
        overrideConfiguration defaultConfiguration configurationToOverrideDefault

    /// Gets all the parent directories of a given path - includes the original path directory too.
    let private getParentDirectories path =
        let rec getParentDirectories parentDirectories (directoryInfo:DirectoryInfo) =
            match directoryInfo with
            | null -> parentDirectories
            | _ -> getParentDirectories (directoryInfo::parentDirectories) directoryInfo.Parent

        DirectoryInfo path |> getParentDirectories []

    /// Overrides the default config with user defined config files.
    /// The configs can be in any directory between the root directory and the projects directory.
    /// The closer they are to the project directory the higher precedence they have.
    /// e.g. if the project directory is C:\User\Matt\Project then a config file found in 
    /// C:\User\ will be loaded before and overridden by a config file found in C:\User\Matt\.
    let private loadUserConfigFiles projectFilePath defaultConfig =
        let projectFileDirectory = Path.GetDirectoryName projectFilePath
        let subdirectories = getParentDirectories projectFileDirectory |> List.map (fun x -> x.FullName)

        let rec loadAllConfigs configToOveride = function
            | path::paths ->
                let filename = Path.Combine(path, SettingsFileName)

                if File.Exists(filename) then
                    try
                        let newConfig = 
                            File.ReadAllText filename 
                            |> configuration
                            |> (overrideConfiguration configToOveride)

                        loadAllConfigs newConfig paths
                    with
                    | ConfigurationException(message) ->
                        ConfigurationResult.Failure(FailedToLoadConfig (sprintf "Failed to load config file %s: %s" filename message))
                    | :? System.Xml.XmlException as e ->
                        ConfigurationResult.Failure(FailedToLoadConfig (sprintf "Failed to load config file %s: %s" filename e.Message))
                else
                    loadAllConfigs configToOveride paths
            | [] -> 
                ConfigurationResult.Success(configToOveride)

        loadAllConfigs defaultConfig subdirectories

    /// Loads the FSharpLint configuration for a project given the path to the `.fsproj` file.
    /// It picks up configurations in any directory between the root directory and the project's directory.
    /// The closer they are to the project directory the higher precedence they have.
    /// e.g. if the project directory is C:\User\Matt\Project then a config file found in 
    /// C:\User\ will be loaded before and overridden by a config file found in C:\User\Matt\.
    let loadConfigurationForProject projectFilePath =
        loadUserConfigFiles projectFilePath defaultConfiguration

/// Provides an API for running FSharpLint from within another application. 
[<AutoOpen>]
module Lint =

    open System
    open System.Collections.Generic
    open System.IO
    open Microsoft.Build.Exceptions
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework
    
    /// Reason for the linter failing.
    type LintFailure =
        /// Project file path did not exist on the local filesystem.
        | ProjectFileCouldNotBeFound of string

        /// Received exception when trying to get the list of F# file from the project file.
        | MSBuildFailedToLoadProjectFile of string * InvalidProjectFileException

        /// Failed to load a FSharpLint configuration file.
        | FailedToLoadConfig of string

        /// Failed to analyse a loaded FSharpLint configuration at runtime e.g. invalid hint.
        | RunTimeConfigError

        /// `FSharp.Compiler.Services` failed when trying to parse a file.
        | FailedToParseFile of ParseFile.ParseFileFailure

        /// `FSharp.Compiler.Services` failed when trying to parse one or more files in a project.
        | FailedToParseFilesInProject of ParseFile.ParseFileFailure list

    type Result<'t> = 
        | Success of 't
        | Failure of LintFailure

    /// Provides information on what the linter is currently doing.
    type ProjectProgress =
        /// Started parsing a file (file path).
        | Starting of string

        /// Finished parsing a file (file path).
        | ReachedEnd of string

        /// Failed to parse a file (file path, exception that caused failure).
        | Failed of string * System.Exception

        /// Path of the F# file the progress information is for.
        member this.FilePath() =
            match this with 
            | Starting(f) 
            | ReachedEnd(f)
            | Failed(f, _) -> f
                    
    module LoadPlugins =

        /// Loaded visitors implementing lint rules.
        let plugins =
            typeof<FSharpLint.Rules.Binding.RegisterBindingVisitor>.Assembly
            |> LoadVisitors.loadPlugins

        /// Extracts a list of ast visitors from a general list of visitors.
        let astVisitors (plugins:LoadVisitors.VisitorPlugin list) visitorInfo =
            [ for plugin in plugins do
                match plugin.Visitor with
                | LoadVisitors.Ast(visitor) -> 
                    yield visitor visitorInfo
                | LoadVisitors.PlainText(_) -> () ]
        
        /// Extracts a list of plain text visitors from a general list of visitors.
        let plainTextVisitors (plugins:LoadVisitors.VisitorPlugin list) visitorInfo =
            [ for plugin in plugins do
                match plugin.Visitor with
                | LoadVisitors.Ast(_) -> ()
                | LoadVisitors.PlainText(visitor) -> 
                    yield visitor visitorInfo ]

    type LintInfo =
        { FinishEarly: unit -> bool
          ErrorReceived: LintWarning.Warning -> unit
          ReportLinterProgress: ProjectProgress -> unit
          RulePlugins: LoadVisitors.VisitorPlugin list
          Configuration: Configuration.Configuration
          FSharpVersion: Version }

    let lint lintInfo (parsedFileInfo:Ast.FileParseInfo) =
        if not <| lintInfo.FinishEarly() then
            let postError range error =
                { LintWarning.Info = error
                  LintWarning.Range = range
                  LintWarning.Input = parsedFileInfo.PlainText } |> lintInfo.ErrorReceived

            let visitorInfo = 
                { Ast.FSharpVersion = lintInfo.FSharpVersion
                  Ast.Config = lintInfo.Configuration
                  Ast.PostError = postError }

            let visitPlainText = async {
                    let suppressMessageAttributes =
                        if parsedFileInfo.PlainText.Contains("SuppressMessage") then
                            Ast.getSuppressMessageAttributesFromAst parsedFileInfo.Ast
                        else []
                    let stringLiterals = Ast.getStringLiteralsFromAst parsedFileInfo.Ast

                    for visitor in LoadPlugins.plainTextVisitors lintInfo.RulePlugins visitorInfo do
                        visitor 
                            { Input = parsedFileInfo.PlainText
                              File = parsedFileInfo.File
                              SuppressedMessages = suppressMessageAttributes
                              StringLiterals = stringLiterals }
                }

            let visitAst = async {
                    try
                        LoadPlugins.astVisitors lintInfo.RulePlugins visitorInfo
                        |> Ast.lintFile lintInfo.FinishEarly parsedFileInfo
                    with 
                    | e -> Failed(parsedFileInfo.File, e) |> lintInfo.ReportLinterProgress
                }

            Starting(parsedFileInfo.File) |> lintInfo.ReportLinterProgress

            [visitAst; visitPlainText]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore

            ReachedEnd(parsedFileInfo.File) |> lintInfo.ReportLinterProgress

    let getProjectFileInfo projectFilePath =
        // Setting `FSharpLintEnabled` to `false` is very important as `ProjectCracker` can build the project file,
        // so if run as an msbuild task without this property we'd end up with an infinite loop of builds (taking out the machine).
        let msBuildProperties = ["FSharpLintEnabled", "false"]

        try Success(ProjectCracker.GetProjectOptionsFromProjectFile(projectFilePath, msBuildProperties))
        with
        | :? InvalidProjectFileException as e ->
            Failure(MSBuildFailedToLoadProjectFile(projectFilePath, e))
        | :? FileNotFoundException as e ->
            Failure(MSBuildFailedToLoadProjectFile(projectFilePath, InvalidProjectFileException(e.Message, e)))

    let configFailureToLintFailure = function
        | ConfigurationManagement.FailedToLoadConfig(f) -> FailedToLoadConfig(f)
        | ConfigurationManagement.RunTimeConfigError -> RunTimeConfigError

    let loadConfigurationFilesForProject projectFilePath =
        try
            match ConfigurationManagement.loadConfigurationForProject projectFilePath with
            | ConfigurationManagement.ConfigurationResult.Success(config) -> Success(config)
            | ConfigurationManagement.ConfigurationResult.Failure(x) -> Failure(configFailureToLintFailure x)
        with 
        | Configuration.ConfigurationException(_) -> Failure(RunTimeConfigError)

    let getFailedFiles = function 
        | ParseFile.Failed(failure) -> Some(failure) 
        | _ -> None

    let getParsedFiles = function 
        | ParseFile.Success(file) -> Some(file) 
        | _ -> None

    /// Result of running the linter.
    [<RequireQualifiedAccess>]
    type LintResult = 
        | Success of LintWarning.Warning list
        | Failure of LintFailure

    /// Optional parameters that can be provided to the linter.
    type OptionalLintParameters =
        { /// This function will be called as the linter progresses through the AST of each file.
          /// The linter will stop linting if this function returns true.
          FinishEarly: (unit -> bool) option

          /// Provide your own FSharpLint configuration to the linter.
          /// If not provided the default configuration will be used.
          Configuration: Configuration.Configuration option

          /// This function will be called every time the linter finds a broken rule.
          ReceivedWarning: (LintWarning.Warning -> unit) option }
        
        static member Default = { FinishEarly = None; Configuration = None; ReceivedWarning = None }

    /// If your application has already parsed the F# source files using `FSharp.Compiler.Services` 
    /// you want to lint then this can be used to provide the parsed information to prevent the 
    /// linter from parsing the file again.
    type ParsedFileInformation =
        { /// File represented as an AST.
          Ast: Microsoft.FSharp.Compiler.Ast.ParsedInput

          /// Contents of the file.
          Source: string

          /// Optional results of inferring the types on the AST (allows for a more accurate lint).
          TypeCheckResults: FSharpCheckFileResults option

          /// Version of F# the source code of the file was written in.
          FSharpVersion: Version }
    
    /// Lints an entire F# project by retrieving the files from a given 
    /// path to the `.fsproj` file.
    let lintProject optionalParams projectFilePath progress =
        let lintWarnings = LinkedList<LintWarning.Warning>()

        let projectProgress = match progress with Some(f) -> f | None -> ignore

        let warningReceived (warning:LintWarning.Warning) =
            lintWarnings.AddLast warning |> ignore

            optionalParams.ReceivedWarning |> Option.iter (fun func -> func warning)

        let checker = FSharpChecker.Create()

        let parseFilesInProject config files projectOptions =
            let lintInformation =
                { Configuration = config
                  RulePlugins = LoadPlugins.plugins
                  FinishEarly = match optionalParams.FinishEarly with Some(f) -> f | None -> fun _ -> false
                  ErrorReceived = warningReceived
                  ReportLinterProgress = projectProgress
                  FSharpVersion = System.Version(4, 0) }

            let isIgnoredFile filePath = 
                match config.IgnoreFiles with
                | Some({ Files = ignoreFiles }) -> 
                    Configuration.IgnoreFiles.shouldFileBeIgnored ignoreFiles filePath
                | None -> false

            let parsedFiles =
                files
                |> List.filter (not << isIgnoredFile)
                |> List.map (fun file -> ParseFile.parseFile file config checker (Some(projectOptions)))
                            
            let failedFiles = parsedFiles |> List.choose getFailedFiles

            if List.isEmpty failedFiles then
                parsedFiles
                |> List.choose getParsedFiles
                |> List.iter (lint lintInformation)
                        
                Success()
            else
                Failure(FailedToParseFilesInProject(failedFiles))

        let loadConfigAndParseFilesInProject files projectOptions =
            let config = 
                match optionalParams.Configuration with
                | Some(userSuppliedConfig) -> Success userSuppliedConfig
                | None -> loadConfigurationFilesForProject projectFilePath

            match config with
            | Success(config) -> parseFilesInProject config files projectOptions
            | Failure(x) -> Failure(x)

        match getProjectFileInfo projectFilePath with
        | Success(projectOptions) ->
            let compileFiles = 
                projectOptions.OtherOptions 
                |> Seq.filter (fun x -> x.StartsWith "-" |> not)
                |> Seq.toList

            match loadConfigAndParseFilesInProject compileFiles projectOptions with
            | Success() -> lintWarnings |> Seq.toList |> LintResult.Success 
            | Failure(x) -> LintResult.Failure(x)
        | Failure(x) -> LintResult.Failure(x)
                
    /// Lints F# source code that has already been parsed using 
    /// `FSharp.Compiler.Services` in the calling application.
    let lintParsedSource optionalParams parsedFileInfo =
        let lintWarnings = LinkedList<LintWarning.Warning>()

        let warningReceived (warning:LintWarning.Warning) =
            lintWarnings.AddLast warning |> ignore

            optionalParams.ReceivedWarning |> Option.iter (fun func -> func warning)

        let config = 
            match optionalParams.Configuration with
            | Some(userSuppliedConfig) -> userSuppliedConfig
            | None -> Configuration.defaultConfiguration

        let lintInformation =
            { Configuration = config
              RulePlugins = LoadPlugins.plugins
              FinishEarly = match optionalParams.FinishEarly with Some(f) -> f | None -> fun _ -> false
              ErrorReceived = warningReceived
              ReportLinterProgress = ignore
              FSharpVersion = parsedFileInfo.FSharpVersion }

        let parsedFileInfo =
            { Ast.PlainText = parsedFileInfo.Source
              Ast.Ast = parsedFileInfo.Ast
              Ast.TypeCheckResults = parsedFileInfo.TypeCheckResults
              Ast.File = "/home/user/Dog.Test.fsx" }

        lint lintInformation parsedFileInfo

        lintWarnings |> Seq.toList |> LintResult.Success
            
    /// Lints F# source code.
    let lintSource optionalParams source fsharpVersion =
        let config = 
            match optionalParams.Configuration with
            | Some(userSuppliedConfig) -> userSuppliedConfig
            | None -> Configuration.defaultConfiguration

        let checker = FSharpChecker.Create()

        match ParseFile.parseSource source config checker with
        | ParseFile.Success(parseFileInformation) -> 
            let parsedFileInfo =
                { Source = parseFileInformation.PlainText
                  Ast = parseFileInformation.Ast
                  TypeCheckResults = parseFileInformation.TypeCheckResults
                  FSharpVersion = fsharpVersion }

            lintParsedSource optionalParams parsedFileInfo
        | ParseFile.Failed(failure) -> LintResult.Failure(FailedToParseFile(failure))

    /// Lints an F# file that has already been parsed using 
    /// `FSharp.Compiler.Services` in the calling application. 
    let lintParsedFile optionalParams parsedFileInfo filepath =
        let lintWarnings = LinkedList<LintWarning.Warning>()

        let warningReceived (warning:LintWarning.Warning) =
            lintWarnings.AddLast warning |> ignore

            optionalParams.ReceivedWarning |> Option.iter (fun func -> func warning)

        let config = 
            match optionalParams.Configuration with
            | Some(userSuppliedConfig) -> userSuppliedConfig
            | None -> Configuration.defaultConfiguration
        
        let lintInformation =
            { Configuration = config
              RulePlugins = LoadPlugins.plugins
              FinishEarly = match optionalParams.FinishEarly with Some(f) -> f | None -> fun _ -> false
              ErrorReceived = warningReceived
              ReportLinterProgress = ignore
              FSharpVersion = parsedFileInfo.FSharpVersion }

        let parsedFileInfo =
            { Ast.PlainText = parsedFileInfo.Source
              Ast.Ast = parsedFileInfo.Ast
              Ast.TypeCheckResults = parsedFileInfo.TypeCheckResults
              Ast.File = filepath }

        lint lintInformation parsedFileInfo

        lintWarnings |> Seq.toList |> LintResult.Success
        
    /// Lints an F# file from a given path to the `.fs` file.
    let lintFile optionalParams filepath fsharpVersion =
        let config = 
            match optionalParams.Configuration with
            | Some(userSuppliedConfig) -> userSuppliedConfig
            | None -> Configuration.defaultConfiguration

        let checker = FSharpChecker.Create()

        match ParseFile.parseFile filepath config checker None with
        | ParseFile.Success(astFileParseInfo) -> 
            let parsedFileInfo = 
                { Source = astFileParseInfo.PlainText
                  Ast = astFileParseInfo.Ast
                  TypeCheckResults = astFileParseInfo.TypeCheckResults
                  FSharpVersion = fsharpVersion }

            lintParsedFile optionalParams parsedFileInfo filepath
        | ParseFile.Failed(failure) -> LintResult.Failure(FailedToParseFile(failure))