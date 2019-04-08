namespace FSharpLint.Application

module ConfigurationManager = 

    open System.Collections.Generic
    open System.IO
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open FSharpLint.Framework.Configuration
    open FSharpLint.Rules
    open FSharpLint.Framework.Rules
    
    type RuleConfig<'Config> = {
        enabled : bool
        config : 'Config option
    }
    
    type EnabledConfig = RuleConfig<unit>
    
    type FormattingConfig =
        { typedItemSpacing : RuleConfig<TypedItemSpacing.Config> option
          typePrefixing : EnabledConfig option
          unionDefinitionIndentation : EnabledConfig option
          tupleCommaSpacing : EnabledConfig option
          tupleIndentation : EnabledConfig option
          tupleParentheses : EnabledConfig option
          patternMatchClausesOnNewLine : EnabledConfig option
          patternMatchOrClausesOnNewLine : EnabledConfig option
          patternMatchClauseIndentation : EnabledConfig option
          patternMatchExpressionIndentation : EnabledConfig option }
        
    type ConventionsConfig =
        { recursiveAsyncFunction : EnabledConfig option }

    type TypographyConfig =
        { indentation : RuleConfig<Indentation.Config> option
          maxCharactersOnLine : RuleConfig<MaxCharactersOnLine.Config> option
          trailingWhitespaceOnLine : RuleConfig<TrailingWhitespaceOnLine.Config> option
          maxLinesInFile : RuleConfig<MaxLinesInFile.Config> option
          trailingNewLineInFile : EnabledConfig option
          noTabCharacters : EnabledConfig option }
    
    type Configuration = 
        { ignoreFiles : string []
          formatting : FormattingConfig option
          conventions : ConventionsConfig option
          typography : TypographyConfig option }

    let mergeConfig (baseConfig : string) (overridingConfig : string) =
        let baseConfigJson = JObject.Parse baseConfig
        let overridingConfigJson = JObject.Parse overridingConfig
        baseConfigJson.Merge(overridingConfig)
        baseConfigJson.ToString()

    let parseConfig (configText : string) =
        let settings = JsonSerializerSettings()
        settings.Converters.Add(Converters.StringEnumConverter())
        JsonConvert.DeserializeObject<Configuration> configText
        
    let constructRuleWithConfig rule ruleConfig =
        if ruleConfig.enabled then
            ruleConfig.config |> Option.map (fun config -> rule config)
        else
            None
         
    let constructRuleIfEnabled rule ruleConfig = if ruleConfig.enabled then Some rule else None
        
    let flattenFormattingConfig (config : FormattingConfig) =
        [|
            config.typedItemSpacing |> Option.bind (constructRuleWithConfig TypedItemSpacing.rule)
            config.typePrefixing |> Option.bind (constructRuleIfEnabled TypePrefixing.rule)
            config.unionDefinitionIndentation |> Option.bind (constructRuleIfEnabled UnionDefinitionIndentation.rule)
            config.tupleCommaSpacing |> Option.bind (constructRuleIfEnabled TupleCommaSpacing.rule)
            config.tupleIndentation |> Option.bind (constructRuleIfEnabled TupleIndentation.rule)
            config.tupleParentheses |> Option.bind (constructRuleIfEnabled TupleParentheses.rule)
            config.patternMatchClausesOnNewLine |> Option.bind (constructRuleIfEnabled PatternMatchClausesOnNewLine.rule)
            config.patternMatchOrClausesOnNewLine |> Option.bind (constructRuleIfEnabled PatternMatchOrClausesOnNewLine.rule)
            config.patternMatchClauseIndentation |> Option.bind (constructRuleIfEnabled PatternMatchClauseIndentation.rule)
            config.patternMatchExpressionIndentation |> Option.bind (constructRuleIfEnabled PatternMatchExpressionIndentation.rule)
        |]
        |> Array.choose id
        
    let flattenConventionsConfig (config : ConventionsConfig) =
        [|
            config.recursiveAsyncFunction |> Option.bind (constructRuleIfEnabled RecursiveAsyncFunction.rule)
        |]
        |> Array.choose id
        
    let flattenTypographyConfig (config : TypographyConfig) =
        [|
            config.indentation |> Option.bind (constructRuleWithConfig Indentation.rule)
            config.maxCharactersOnLine |> Option.bind (constructRuleWithConfig MaxCharactersOnLine.rule)
            config.trailingWhitespaceOnLine |> Option.bind (constructRuleWithConfig TrailingWhitespaceOnLine.rule)
            config.maxLinesInFile |> Option.bind (constructRuleWithConfig MaxLinesInFile.rule)
            config.trailingNewLineInFile |> Option.bind (constructRuleIfEnabled TrailingNewLineInFile.rule)
            config.noTabCharacters |> Option.bind (constructRuleIfEnabled NoTabCharacters.rule)
        |]
        |> Array.choose id       
        
    type LoadedRules =
        { astNodeRules : RuleMetadata<AstNodeRuleConfig> []
          lineRules : RuleMetadata<LineRuleConfig> []
          noTabCharactersRule : RuleMetadata<NoTabCharactersRuleConfig> option
          indentationRule : RuleMetadata<IndentationRuleConfig> option }
        
    let flattenConfig (config : Configuration) =
        let allRules =
            [|
                config.formatting |> Option.map flattenFormattingConfig |> Option.toArray |> Array.concat    
                config.conventions |> Option.map flattenConventionsConfig |> Option.toArray |> Array.concat    
                config.typography |> Option.map flattenTypographyConfig |> Option.toArray |> Array.concat
            |] |> Array.concat
            
        let astNodeRules = ResizeArray()
        let lineRules = ResizeArray()
        let mutable indentationRule = None
        let mutable noTabCharactersRule = None
        allRules
        |> Array.iter (function
            | AstNodeRule rule -> astNodeRules.Add rule
            | LineRule rule -> lineRules.Add(rule)
            | IndentationRule rule -> indentationRule <- Some rule
            | NoTabCharactersRule rule -> noTabCharactersRule <- Some rule)
        
        { LoadedRules.astNodeRules = astNodeRules.ToArray()
          lineRules = lineRules.ToArray()
          indentationRule = indentationRule
          noTabCharactersRule = noTabCharactersRule }
    
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