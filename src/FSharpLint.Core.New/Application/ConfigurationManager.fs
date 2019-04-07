namespace FSharpLint.Application

module ConfigurationManager = 

    open System.Collections.Generic
    open System.IO
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open FSharpLint.Framework.Configuration
    open FSharpLint.Rules
    
    type FormattingConfig =
        { typedItemSpacing : TypedItemSpacing.Config option
          typePrefixing : bool option
          unionDefinitionIndentation : bool option
          tupleCommaSpacing : bool option
          tupleIndentation : bool option
          tupleParentheses : bool option
          patternMatchClausesOnNewLine : bool option
          patternMatchOrClausesOnNewLine : bool option
          patternMatchClauseIndentation : bool option
          patternMatchExpressionIndentation : bool option }

    type Configuration = 
        { ignoreFiles : string []
          formatting : FormattingConfig option }

    let mergeConfig (baseConfig : string) (overridingConfig : string) =
        let baseConfigJson = JObject.Parse baseConfig
        let overridingConfigJson = JObject.Parse overridingConfig
        baseConfigJson.Merge(overridingConfig)
        baseConfigJson.ToString()

    let parseConfig (configText : string) =
        let settings = JsonSerializerSettings()
        settings.Converters.Add(Converters.StringEnumConverter())
        JsonConvert.DeserializeObject<Configuration> configText
        
    let constructRuleIfEnabled rule = function
        | Some true -> Some rule
        | _ -> None
        
    let flattenFormattingConfig (config : FormattingConfig) =
        [|
            config.typedItemSpacing |> Option.map TypedItemSpacing.rule
            config.typePrefixing |> constructRuleIfEnabled TypePrefixing.rule
            config.unionDefinitionIndentation |> constructRuleIfEnabled UnionDefinitionIndentation.rule
            config.tupleCommaSpacing |> constructRuleIfEnabled TupleCommaSpacing.rule
            config.tupleIndentation |> constructRuleIfEnabled TupleIndentation.rule
            config.tupleParentheses |> constructRuleIfEnabled TupleParentheses.rule
            config.patternMatchClausesOnNewLine |> constructRuleIfEnabled PatternMatchClausesOnNewLine.rule
            config.patternMatchOrClausesOnNewLine |> constructRuleIfEnabled PatternMatchOrClausesOnNewLine.rule
            config.patternMatchClauseIndentation |> constructRuleIfEnabled PatternMatchClauseIndentation.rule
            config.patternMatchExpressionIndentation |> constructRuleIfEnabled PatternMatchExpressionIndentation.rule
        |]
        |> Array.choose id
        
    let flattenConfig (config : Configuration) =
        config.formatting
        |> Option.map flattenFormattingConfig
        |> Option.toArray
        |> Array.concat    
    
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
            try File.ReadAllText filePath |> Some
            with _ -> None
        else
            None

    /// Loads and stores configurations in memory so that they can easily be modified
    /// and written back out to disk.
    /// Intended to allow for all the configuration files for all the projects in a solution
    /// to be grouped in a single place where they can be modified.
    type ConfigurationManager() =
        let loadedConfigs = Dictionary<string, string>()

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
            |> List.fold mergeConfig defaultConfiguration