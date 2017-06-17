namespace FSharpLint.Application

module ConfigurationManager =

    open System.Collections.Generic
    open System.IO
    open FSharpLint.Framework.Configuration

    /// Gets all the parent directories of a given path - includes the original path directory too.
    let private getParentDirectories path =
        let rec getParentDirectories parentDirectories (directoryInfo:DirectoryInfo) =
            match directoryInfo with
            | null -> parentDirectories
            | _ -> getParentDirectories (directoryInfo.FullName::parentDirectories) directoryInfo.Parent

        DirectoryInfo path |> getParentDirectories []

    [<Literal>]
    let SettingsFileName = "Settings.FSharpLint"

    /// Tries to load a config from disk.
    /// If it fails to load the config any exception will be swallowed and `None` returned.
    /// If the file does not exist `None` will be returned.
    let private tryLoadConfig filePath =
        if File.Exists(filePath) then
            try File.ReadAllText filePath |> configuration |> Some
            with _ -> None
        else
            None

    let getConfigurationForFile filePath =
        let tryGetConfigForDirectory directoryPath =
            let configFilePath = Path.Combine(directoryPath, SettingsFileName)
            tryLoadConfig configFilePath

        Path.GetDirectoryName filePath
        |> getParentDirectories
        |> List.choose tryGetConfigForDirectory
        |> List.fold overrideConfiguration defaultConfiguration

    /// Loads and stores configurations in memory so that they can easily be modified
    /// and written back out to disk.
    /// Intended to allow for all the configuration files for all the projects in a solution
    /// to be grouped in a single place where they can be modified.
    type ConfigurationManager() =
        let loadedConfigs = Dictionary<string, Configuration>()

        member __.LoadConfigurationForProject(projectFilePath) =
            let getConfig (directory) =
                let filePath = Path.Combine(directory, SettingsFileName)

                if loadedConfigs.ContainsKey directory then None
                else
                    match tryLoadConfig filePath with
                    | Some(config) -> Some(directory, config)
                    | None -> None

            Path.GetDirectoryName projectFilePath
            |> getParentDirectories
            |> List.choose getConfig
            |> List.iter loadedConfigs.Add

        member __.GetConfigurationForProject(projectFilePath) =
            let tryGetConfig dir =
                match loadedConfigs.TryGetValue(dir) with
                | true, config -> Some(config)
                | false, _ -> None

            Path.GetDirectoryName projectFilePath
            |> getParentDirectories
            |> List.choose tryGetConfig
            |> List.fold overrideConfiguration defaultConfiguration