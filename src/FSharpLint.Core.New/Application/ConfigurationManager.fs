module FSharpLint.Application.ConfigurationManager

open System.Collections.Generic
 open System.IO
 open Newtonsoft.Json
 open Newtonsoft.Json.Linq
 open FSharpLint.Rules
 open FSharpLint.Framework
 open FSharpLint.Framework.Configuration
 open FSharpLint.Framework.Rules
 open FSharpLint.Framework.HintParser

type RuleConfig<'Config> = {
    enabled : bool
    config : 'Config option
}

type EnabledConfig = RuleConfig<unit>

let constructRuleIfEnabled rule ruleConfig = if ruleConfig.enabled then Some rule else None
    
let constructRuleWithConfig rule ruleConfig =
    if ruleConfig.enabled then
        ruleConfig.config |> Option.map (fun config -> rule config)
    else
        None   

type TupleFormattingConfig =
    { tupleCommaSpacing : EnabledConfig option
      tupleIndentation : EnabledConfig option
      tupleParentheses : EnabledConfig option }
with
    member this.Flatten() =
        [|
            this.tupleCommaSpacing |> Option.bind (constructRuleIfEnabled TupleCommaSpacing.rule)
            this.tupleIndentation |> Option.bind (constructRuleIfEnabled TupleIndentation.rule)
            this.tupleParentheses |> Option.bind (constructRuleIfEnabled TupleParentheses.rule)
        |] |> Array.choose id

type PatternMatchFormattingConfig =
    { patternMatchClausesOnNewLine : EnabledConfig option
      patternMatchOrClausesOnNewLine : EnabledConfig option
      patternMatchClauseIndentation : EnabledConfig option
      patternMatchExpressionIndentation : EnabledConfig option }
with
    member this.Flatten() =
        [|
            this.patternMatchClausesOnNewLine |> Option.bind (constructRuleIfEnabled PatternMatchClausesOnNewLine.rule)
            this.patternMatchOrClausesOnNewLine |> Option.bind (constructRuleIfEnabled PatternMatchOrClausesOnNewLine.rule)
            this.patternMatchClauseIndentation |> Option.bind (constructRuleIfEnabled PatternMatchClauseIndentation.rule)
            this.patternMatchExpressionIndentation |> Option.bind (constructRuleIfEnabled PatternMatchExpressionIndentation.rule)
        |] |> Array.choose id   
    
type FormattingConfig =
    { typedItemSpacing : RuleConfig<TypedItemSpacing.Config> option
      typePrefixing : EnabledConfig option
      unionDefinitionIndentation : EnabledConfig option
      moduleDeclSpacing : EnabledConfig option
      classMemberSpacing : EnabledConfig option
      tupleFormatting : TupleFormattingConfig option
      patternMatchFormatting : PatternMatchFormattingConfig option }
with
    member this.Flatten() =
        [|
            this.typedItemSpacing |> Option.bind (constructRuleWithConfig TypedItemSpacing.rule) |> Option.toArray
            this.typePrefixing |> Option.bind (constructRuleIfEnabled TypePrefixing.rule) |> Option.toArray
            this.unionDefinitionIndentation |> Option.bind (constructRuleIfEnabled UnionDefinitionIndentation.rule) |> Option.toArray
            this.moduleDeclSpacing |> Option.bind (constructRuleIfEnabled ModuleDeclSpacing.rule) |> Option.toArray
            this.classMemberSpacing |> Option.bind (constructRuleIfEnabled ClassMemberSpacing.rule) |> Option.toArray
            this.tupleFormatting |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            this.patternMatchFormatting |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
        |] |> Array.concat

type RaiseWithTooManyArgsConfig =
    { raiseWithSingleArgument : EnabledConfig option
      nullArgWithSingleArgument : EnabledConfig option
      invalidOpWithSingleArgument : EnabledConfig option
      invalidArgWithTwoArguments : EnabledConfig option
      failwithfWithArgumentsMatchingFormatString : EnabledConfig option }
with
    member this.Flatten() =
        [|
            this.raiseWithSingleArgument |> Option.bind (constructRuleIfEnabled RaiseWithSingleArgument.rule) |> Option.toArray
            this.nullArgWithSingleArgument |> Option.bind (constructRuleIfEnabled NullArgWithSingleArgument.rule) |> Option.toArray
            this.invalidOpWithSingleArgument |> Option.bind (constructRuleIfEnabled InvalidOpWithSingleArgument.rule) |> Option.toArray
            this.invalidArgWithTwoArguments |> Option.bind (constructRuleIfEnabled InvalidArgWithTwoArguments.rule) |> Option.toArray
            this.failwithfWithArgumentsMatchingFormatString |> Option.bind (constructRuleIfEnabled FailwithfWithArgumentsMatchingFormatString.rule) |> Option.toArray
        |] |> Array.concat
 
type SourceLengthConfig =
    { maxLinesInLambdaFunction : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInMatchLambdaFunction : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInValue : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInFunction : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInMember : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInConstructor : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInProperty : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInModule : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInRecord : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInEnum : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInUnion : RuleConfig<Helper.SourceLength.Config> option
      maxLinesInClass : RuleConfig<Helper.SourceLength.Config> option }
with
    member this.Flatten() =
        [|
            this.maxLinesInLambdaFunction |> Option.bind (constructRuleWithConfig MaxLinesInLambdaFunction.rule) |> Option.toArray
            this.maxLinesInMatchLambdaFunction |> Option.bind (constructRuleWithConfig MaxLinesInMatchLambdaFunction.rule) |> Option.toArray
            this.maxLinesInValue |> Option.bind (constructRuleWithConfig MaxLinesInValue.rule) |> Option.toArray
            this.maxLinesInFunction |> Option.bind (constructRuleWithConfig MaxLinesInFunction.rule) |> Option.toArray
            this.maxLinesInMember |> Option.bind (constructRuleWithConfig MaxLinesInMember.rule) |> Option.toArray
            this.maxLinesInConstructor |> Option.bind (constructRuleWithConfig MaxLinesInConstructor.rule) |> Option.toArray
            this.maxLinesInProperty |> Option.bind (constructRuleWithConfig MaxLinesInProperty.rule) |> Option.toArray
            this.maxLinesInModule |> Option.bind (constructRuleWithConfig MaxLinesInModule.rule) |> Option.toArray
            this.maxLinesInRecord |> Option.bind (constructRuleWithConfig MaxLinesInRecord.rule) |> Option.toArray
            this.maxLinesInEnum |> Option.bind (constructRuleWithConfig MaxLinesInEnum.rule) |> Option.toArray
            this.maxLinesInUnion |> Option.bind (constructRuleWithConfig MaxLinesInUnion.rule) |> Option.toArray
            this.maxLinesInClass |> Option.bind (constructRuleWithConfig MaxLinesInClass.rule) |> Option.toArray
        |] |> Array.concat

type NamesConfig =
    { interfaceNames : RuleConfig<NamingConfig> option
      exceptionNames : RuleConfig<NamingConfig> option
      typeNames : RuleConfig<NamingConfig> option
      recordFieldNames : RuleConfig<NamingConfig> option
      enumCasesNames : RuleConfig<NamingConfig> option
      unionCasesNames : RuleConfig<NamingConfig> option
      moduleNames : RuleConfig<NamingConfig> option
      literalNames : RuleConfig<NamingConfig> option
      namespaceNames : RuleConfig<NamingConfig> option
      memberNames : RuleConfig<NamingConfig> option
      parameterNames : RuleConfig<NamingConfig> option
      measureTypeNames : RuleConfig<NamingConfig> option
      activePatternNames : RuleConfig<NamingConfig> option
      publicValuesNames : RuleConfig<NamingConfig> option
      nonPublicValuesNames : RuleConfig<NamingConfig> option }
with
    member this.Flatten() =
        [|
            this.interfaceNames |> Option.bind (constructRuleWithConfig InterfaceNames.rule) |> Option.toArray
            this.exceptionNames |> Option.bind (constructRuleWithConfig ExceptionNames.rule) |> Option.toArray
            this.typeNames |> Option.bind (constructRuleWithConfig TypeNames.rule) |> Option.toArray
            this.recordFieldNames |> Option.bind (constructRuleWithConfig RecordFieldNames.rule) |> Option.toArray
            this.enumCasesNames |> Option.bind (constructRuleWithConfig EnumCasesNames.rule) |> Option.toArray
            this.unionCasesNames |> Option.bind (constructRuleWithConfig UnionCasesNames.rule) |> Option.toArray
            this.moduleNames |> Option.bind (constructRuleWithConfig ModuleNames.rule) |> Option.toArray
            this.literalNames |> Option.bind (constructRuleWithConfig LiteralNames.rule) |> Option.toArray
            this.namespaceNames |> Option.bind (constructRuleWithConfig NamespaceNames.rule) |> Option.toArray
            this.memberNames |> Option.bind (constructRuleWithConfig MemberNames.rule) |> Option.toArray
            this.parameterNames |> Option.bind (constructRuleWithConfig ParameterNames.rule) |> Option.toArray
            this.measureTypeNames |> Option.bind (constructRuleWithConfig MeasureTypeNames.rule) |> Option.toArray
            this.activePatternNames |> Option.bind (constructRuleWithConfig ActivePatternNames.rule) |> Option.toArray
            this.publicValuesNames |> Option.bind (constructRuleWithConfig PublicValuesNames.rule) |> Option.toArray
            this.nonPublicValuesNames |> Option.bind (constructRuleWithConfig NonPublicValuesNames.rule) |> Option.toArray
        |] |> Array.concat

type NumberOfItemsConfig =
    { maxNumberOfItemsInTuple : RuleConfig<MaxNumberOfItemsInTuple.Config> option 
      maxNumberOfFunctionParameters : RuleConfig<MaxNumberOfFunctionParameters.Config> option 
      maxNumberOfMembers : RuleConfig<MaxNumberOfMembers.Config> option 
      maxNumberOfBooleanOperatorsInCondition : RuleConfig<MaxNumberOfBooleanOperatorsInCondition.Config> option }
 with
    member this.Flatten() =
         [|
            this.maxNumberOfItemsInTuple |> Option.bind (constructRuleWithConfig MaxNumberOfItemsInTuple.rule) |> Option.toArray   
            this.maxNumberOfFunctionParameters |> Option.bind (constructRuleWithConfig MaxNumberOfFunctionParameters.rule) |> Option.toArray   
            this.maxNumberOfMembers |> Option.bind (constructRuleWithConfig MaxNumberOfMembers.rule) |> Option.toArray   
            this.maxNumberOfBooleanOperatorsInCondition |> Option.bind (constructRuleWithConfig MaxNumberOfBooleanOperatorsInCondition.rule) |> Option.toArray   
         |] |> Array.concat

type BindingConfig =
    { favourIgnoreOverLetWild : EnabledConfig option 
      wildcardNamedWithAsPattern : EnabledConfig option 
      uselessBinding : EnabledConfig option 
      tupleOfWildcards : EnabledConfig option }
 with
    member this.Flatten() =
         [|
            this.favourIgnoreOverLetWild |> Option.bind (constructRuleIfEnabled FavourIgnoreOverLetWild.rule) |> Option.toArray
            this.wildcardNamedWithAsPattern |> Option.bind (constructRuleIfEnabled WildcardNamedWithAsPattern.rule) |> Option.toArray
            this.uselessBinding |> Option.bind (constructRuleIfEnabled UselessBinding.rule) |> Option.toArray
            this.tupleOfWildcards |> Option.bind (constructRuleIfEnabled TupleOfWildcards.rule) |> Option.toArray
         |] |> Array.concat

type ConventionsConfig =
    { recursiveAsyncFunction : EnabledConfig option
      redundantNewKeyword : EnabledConfig option
      nestedStatements : RuleConfig<NestedStatements.Config> option
      reimplementsFunction : EnabledConfig option
      canBeReplacedWithComposition : EnabledConfig option
      raiseWithTooManyArgs : RaiseWithTooManyArgsConfig option
      sourceLength : SourceLengthConfig option
      namesConfig : NamesConfig option
      numberOfItems : NumberOfItemsConfig option
      binding : BindingConfig option }
with
    member this.Flatten() =
        [|
            this.recursiveAsyncFunction |> Option.bind (constructRuleIfEnabled RecursiveAsyncFunction.rule) |> Option.toArray
            this.redundantNewKeyword |> Option.bind (constructRuleIfEnabled RedundantNewKeyword.rule) |> Option.toArray
            this.nestedStatements |> Option.bind (constructRuleWithConfig NestedStatements.rule) |> Option.toArray
            this.reimplementsFunction |> Option.bind (constructRuleIfEnabled ReimplementsFunction.rule) |> Option.toArray
            this.canBeReplacedWithComposition |> Option.bind (constructRuleIfEnabled CanBeReplacedWithComposition.rule) |> Option.toArray
            this.raiseWithTooManyArgs |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            this.sourceLength |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            this.namesConfig |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
        |] |> Array.concat
    
type TypographyConfig =
    { indentation : RuleConfig<Indentation.Config> option
      maxCharactersOnLine : RuleConfig<MaxCharactersOnLine.Config> option
      trailingWhitespaceOnLine : RuleConfig<TrailingWhitespaceOnLine.Config> option
      maxLinesInFile : RuleConfig<MaxLinesInFile.Config> option
      trailingNewLineInFile : EnabledConfig option
      noTabCharacters : EnabledConfig option }
with
    member this.Flatten() =
         [|
            this.indentation |> Option.bind (constructRuleWithConfig Indentation.rule) |> Option.toArray
            this.maxCharactersOnLine |> Option.bind (constructRuleWithConfig MaxCharactersOnLine.rule) |> Option.toArray
            this.trailingWhitespaceOnLine |> Option.bind (constructRuleWithConfig TrailingWhitespaceOnLine.rule) |> Option.toArray
            this.maxLinesInFile |> Option.bind (constructRuleWithConfig MaxLinesInFile.rule) |> Option.toArray
            this.trailingNewLineInFile |> Option.bind (constructRuleIfEnabled TrailingNewLineInFile.rule) |> Option.toArray
            this.noTabCharacters |> Option.bind (constructRuleIfEnabled NoTabCharacters.rule) |> Option.toArray
         |] |> Array.concat
         

type Configuration = 
    { ignoreFiles : string []
      formatting : FormattingConfig option
      conventions : ConventionsConfig option
      typography : TypographyConfig option
      hints : string [] }

let mergeConfig (baseConfig : string) (overridingConfig : string) =
    let baseConfigJson = JObject.Parse baseConfig
    let overridingConfigJson = JObject.Parse overridingConfig
    baseConfigJson.Merge(overridingConfig)
    baseConfigJson.ToString()

let parseConfig (configText : string) =
    let settings = JsonSerializerSettings()
    settings.Converters.Add(Converters.StringEnumConverter())
    JsonConvert.DeserializeObject<Configuration> configText

     
type LineRules =
    { genericLineRules : RuleMetadata<LineRuleConfig> []
      noTabCharactersRule : RuleMetadata<NoTabCharactersRuleConfig> option
      indentationRule : RuleMetadata<IndentationRuleConfig> option }
    
type LoadedRules =
    { ignoreFiles : string []
      astNodeRules : RuleMetadata<AstNodeRuleConfig> []
      lineRules : LineRules }
    
let private parseHints (hints:string []) =
    let parseHint hint =
        match FParsec.CharParsers.run HintParser.phint hint with
        | FParsec.CharParsers.Success(hint, _, _) -> hint
        | FParsec.CharParsers.Failure(error, _, _) ->
            failwithf "Failed to parse hint '%s'" hint

    hints
    |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Array.map parseHint
    |> Array.toList
    |> MergeSyntaxTrees.mergeHints

let flattenConfig (config : Configuration) =
    let allRules =
        [|
            config.formatting |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.conventions |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.typography |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            HintMatcher.rule { HintMatcher.Config.hintTrie = parseHints config.hints } |> Array.singleton
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
    
    { LoadedRules.ignoreFiles = config.ignoreFiles
      astNodeRules = astNodeRules.ToArray()
      lineRules =
          { genericLineRules = lineRules.ToArray()
            indentationRule = indentationRule
            noTabCharactersRule = noTabCharactersRule } }

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