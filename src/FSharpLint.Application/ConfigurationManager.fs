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

module ConfigurationManager =

    open System.IO
    open FSharpLint.Application.ConfigurationManagement
    open FSharpLint.Framework.LoadVisitors
    open FSharpLint.Framework.Configuration

    /// Gets config checkers with a memoized approach 
    /// (only retrieved from the assembly once).
    let private configCheckers = 
        let configCheckers = ref None

        fun () ->
            match !configCheckers with
            | None ->
                let checkers =
                    FSharpLint.Framework.LoadVisitors.rulesAssembly
                        |> FSharpLint.Framework.LoadVisitors.loadConfigCheckers

                configCheckers := Some(checkers)

                checkers
            | Some(checkers) -> checkers

    /// Check a config using checkers implemented by visitors.
    /// These are used to verify hints are valid etc.
    let private checkConfig config =
        try
            let configFailures = configCheckers() |> (checkConfigsForFailures config)

            match configFailures with
            | [] -> ConfigurationResult.Success(config)
            | firstFailure::_ -> ConfigurationResult.Failure(FailedToLoadConfig(firstFailure)) 
        with
        | ConfigurationException(message) ->
            ConfigurationResult.Failure(FailedToLoadConfig("Failed to load default config: " + message)) 

    /// Gets all the parent directories of a given path - includes the original path directory too.
    let private getParentDirectories path =
        let rec getParentDirectories parentDirectories (directoryInfo:DirectoryInfo) =
            match directoryInfo with
                | null -> parentDirectories
                | _ -> getParentDirectories (directoryInfo::parentDirectories) directoryInfo.Parent

        DirectoryInfo path |> getParentDirectories []

    [<Literal>]
    let SettingsFileName = "Settings.FSharpLint"

    /// Tries to load a config from disk.
    /// If it fails to load the config any exception will be swallowed and `None` returned.
    /// If the file does not exist `None` will be returned.
    let private tryLoadConfig filePath =
        if File.Exists(filePath) then
            try
                let newConfig = File.ReadAllText filePath |> configuration

                match checkConfig newConfig with
                    | ConfigurationResult.Success(config) -> Some(config)
                    | ConfigurationResult.Failure(_) -> None
            with
                | _ -> None
        else
            None

    /// Loads and stores configurations in memory so that they can easily be modified
    /// and written back out to disk.
    /// Intended to allow for all the configuration files for all the projects in a solution
    /// to be grouped in a single place where they can be modified.
    type ConfigurationManager() =
        let loadedConfigs = System.Collections.Generic.Dictionary<DirectoryInfo, Configuration>()
                
        let checkedDefaultConfiguration = checkConfig defaultConfiguration      

        member this.LoadConfigurationForProject(projectFilePath) =
            let getConfig (directory:DirectoryInfo) =
                let filePath = Path.Combine(directory.FullName, SettingsFileName)

                if loadedConfigs.ContainsKey directory then
                    None
                else
                    match tryLoadConfig filePath with
                    | Some(config) -> Some(directory, config)
                    | None -> None
                
            Path.GetDirectoryName projectFilePath 
                |> getParentDirectories
                |> List.choose getConfig
                |> List.iter loadedConfigs.Add

        member this.GetConfigurationForProject(projectFilePath) =
            let tryGetConfig dir =
                match loadedConfigs.TryGetValue(dir) with
                | true, config -> Some(config)
                | false, _ -> None

            Path.GetDirectoryName projectFilePath 
                |> getParentDirectories
                |> List.choose tryGetConfig
                |> List.fold overrideConfiguration defaultConfiguration