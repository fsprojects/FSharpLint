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

module ProjectFile =

    open System.IO
    open System.Linq
    open Microsoft.Build.Tasks
    open Microsoft.Build.Framework
    open Microsoft.Build.BuildEngine
    open Microsoft.FSharp.Compiler.SourceCodeServices 
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let SettingsFileName = "Settings.FSharpLint"

    type Error =
        | ProjectFileCouldNotBeFound of string
        | MSBuildFailedToLoadProjectFile of string * Microsoft.Build.Exceptions.InvalidProjectFileException
        | UnableToFindProjectOutputPath of string
        | UnableToFindReferencedProject of string
        | FailedToLoadConfig of string
        | RunTimeConfigError
        | FailedToResolveReferences

    type Result<'TSuccess> = 
        | Success of 'TSuccess
        | Failure of Error

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
    let private overideDefaultConfig projectFilePath defaultConfig checkConfig =
        let subdirectories = getParentDirectories projectFilePath |> List.map (fun x -> x.FullName)

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

    let loadRulesAssembly () =
        let directory =
            System.Reflection.Assembly.GetExecutingAssembly().Location
                |> System.IO.Path.GetDirectoryName

        let rulesAssembly = sprintf "%s%c%s" directory System.IO.Path.DirectorySeparatorChar "FSharpLint.Rules.dll"

        System.Reflection.Assembly.LoadFrom rulesAssembly

    let loadConfigForProject projectFilePath =
        let configCheckers = 
            loadRulesAssembly()
                |> FSharpLint.Framework.LoadVisitors.loadConfigCheckers

        let checkConfig config =
            let configFailures = configCheckers 
                                    |> (FSharpLint.Framework.LoadVisitors.checkConfigsForFailures config)

            if List.length configFailures = 0 then
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
            | Success(config) -> overideDefaultConfig projectFilePath config checkConfig
            | x -> x