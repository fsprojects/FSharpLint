/// Loads configuration file from JSON into an object.
module FSharpLint.Framework.Configuration

open System
open System.IO
open System.Reflection
open Newtonsoft.Json
open FSharpLint.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Framework.HintParser
open FSharpLint.Rules

[<Literal>]
let internal SettingsFileName = "fsharplint.json"

exception internal ConfigurationException of string

module internal FSharpJsonConverter =
    open Microsoft.FSharp.Reflection

    type OptionConverter() =
        inherit JsonConverter()

        override x.CanConvert(t) =
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>

        override x.WriteJson(writer, value, serializer) =
            let value =
                if isNull value then null
                else
                    let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
                    fields.[0]
            serializer.Serialize(writer, value)

        override x.ReadJson(reader, t, _, serializer) =
            let innerType = t.GetGenericArguments().[0]
            let innerType =
                if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
                else innerType
            let value = serializer.Deserialize(reader, innerType)
            let cases = FSharpType.GetUnionCases(t)
            if isNull value then FSharpValue.MakeUnion(cases.[0], [||])
            else FSharpValue.MakeUnion(cases.[1], [|value|])

    let private converters =
        [|
            OptionConverter() :> JsonConverter
        |]

    let serializerSettings =
        let settings = JsonSerializerSettings()
        settings.NullValueHandling <- NullValueHandling.Ignore
        settings.Converters <- converters
        settings

module internal IgnoreFiles =

    open System.Text.RegularExpressions

    type IsDirectory = | IsDirectory of bool

    [<NoComparison>]
    type Ignore =
        | Ignore of Regex list * IsDirectory
        | Negate of Regex list * IsDirectory

    let parseIgnorePath (path:string) =
        let globToRegex glob =
            Regex(
                "^" + Regex.Escape(glob).Replace(@"\*", ".*").Replace(@"\?", ".") + "$",
                RegexOptions.IgnoreCase ||| RegexOptions.Singleline)

        let isDirectory = path.EndsWith("/")

        let getRegexSegments (path:string) =
            path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map globToRegex

        if path.StartsWith("!") then
            getRegexSegments (path.Substring(1))
            |> Array.toList
            |> fun segments -> Negate(segments, IsDirectory(isDirectory))
        else
            getRegexSegments (if path.StartsWith(@"\!") then path.Substring(1) else path)
            |> Array.toList
            |> fun segments -> Ignore(segments, IsDirectory(isDirectory))

    let private pathMatchesGlob (globs:Regex list) (path:string list) isDirectory =
        let rec getRemainingGlobSeqForMatches pathSegment (globSeqs:Regex list list) =
            globSeqs |> List.choose (function
                | globSegment::remaining when globSegment.IsMatch(pathSegment) -> Some remaining
                | _ -> None)

        let rec doesGlobSeqMatchPathSeq remainingPath currentlyMatchingGlobs =
            match remainingPath with
            | [_] when isDirectory -> false
            | currentSegment::remaining ->
                let currentlyMatchingGlobs = globs::currentlyMatchingGlobs

                let currentlyMatchingGlobs = getRemainingGlobSeqForMatches currentSegment currentlyMatchingGlobs

                let aGlobWasCompletelyMatched = currentlyMatchingGlobs |> List.exists List.isEmpty

                let matched = aGlobWasCompletelyMatched && (isDirectory || (not isDirectory && List.isEmpty remaining))

                if matched then true
                else doesGlobSeqMatchPathSeq remaining currentlyMatchingGlobs
            | [] -> false

        doesGlobSeqMatchPathSeq path []

    let shouldFileBeIgnored (ignorePaths:Ignore list) (filePath:string) =
        let segments = filePath.Split Path.DirectorySeparatorChar |> Array.toList

        ignorePaths |> List.fold (fun isCurrentlyIgnored ignoreGlob ->
            match ignoreGlob with
            | Ignore(glob, IsDirectory(isDirectory))
                when not isCurrentlyIgnored && pathMatchesGlob glob segments isDirectory -> true
            | Negate(glob, IsDirectory(isDirectory))
                when isCurrentlyIgnored && pathMatchesGlob glob segments isDirectory -> false
            | _ -> isCurrentlyIgnored) false

// Non-standard record field naming for config serialization.
// fsharplint:disable RecordFieldNames
type RuleConfig<'Config> = {
    Enabled : bool
    Config : 'Config option
}

type EnabledConfig = RuleConfig<unit>

let private constructRuleIfEnabled rule ruleConfig = if ruleConfig.Enabled then Some rule else None

let private constructRuleWithConfig rule ruleConfig =
    if ruleConfig.Enabled then
        ruleConfig.Config |> Option.map rule
    else
        None

let private getOrEmptyList hints = hints |> Option.defaultValue [||]

type HintConfig = {
    add : string [] option
    ignore : string [] option
}

type GlobalConfig = {
    numIndentationSpaces : int option
}

type Configuration =
    { Global : GlobalConfig option
      ignoreFiles : string [] option
      Hints : HintConfig option
      TypedItemSpacing : RuleConfig<TypedItemSpacing.Config> option
      TypePrefixing : EnabledConfig option
      UnionDefinitionIndentation : EnabledConfig option
      ModuleDeclSpacing : EnabledConfig option
      ClassMemberSpacing : EnabledConfig option
      TupleCommaSpacing : EnabledConfig option
      TupleIndentation : EnabledConfig option
      TupleParentheses : EnabledConfig option
      PatternMatchClausesOnNewLine : EnabledConfig option
      PatternMatchOrClausesOnNewLine : EnabledConfig option
      PatternMatchClauseIndentation : EnabledConfig option
      PatternMatchExpressionIndentation : EnabledConfig option
      RecursiveAsyncFunction : EnabledConfig option
      RedundantNewKeyword : EnabledConfig option
      NestedStatements : RuleConfig<NestedStatements.Config> option
      ReimplementsFunction : EnabledConfig option
      CanBeReplacedWithComposition : EnabledConfig option
      RaiseWithSingleArgument : EnabledConfig option
      FailwithWithSingleArgument : EnabledConfig option
      NullArgWithSingleArgument : EnabledConfig option
      InvalidOpWithSingleArgument : EnabledConfig option
      InvalidArgWithTwoArguments : EnabledConfig option
      FailwithfWithArgumentsMatchingFormatString : EnabledConfig option
      MaxLinesInLambdaFunction : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInMatchLambdaFunction : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInValue : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInFunction : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInMember : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInConstructor : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInProperty : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInModule : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInRecord : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInEnum : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInUnion : RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInClass : RuleConfig<Helper.SourceLength.Config> option
      InterfaceNames : RuleConfig<NamingConfig> option
      ExceptionNames : RuleConfig<NamingConfig> option
      TypeNames : RuleConfig<NamingConfig> option
      RecordFieldNames : RuleConfig<NamingConfig> option
      EnumCasesNames : RuleConfig<NamingConfig> option
      UnionCasesNames : RuleConfig<NamingConfig> option
      ModuleNames : RuleConfig<NamingConfig> option
      LiteralNames : RuleConfig<NamingConfig> option
      NamespaceNames : RuleConfig<NamingConfig> option
      MemberNames : RuleConfig<NamingConfig> option
      ParameterNames : RuleConfig<NamingConfig> option
      MeasureTypeNames : RuleConfig<NamingConfig> option
      ActivePatternNames : RuleConfig<NamingConfig> option
      PublicValuesNames : RuleConfig<NamingConfig> option
      NonPublicValuesNames : RuleConfig<NamingConfig> option
      MaxNumberOfItemsInTuple : RuleConfig<Helper.NumberOfItems.Config> option
      MaxNumberOfFunctionParameters : RuleConfig<Helper.NumberOfItems.Config> option
      MaxNumberOfMembers : RuleConfig<Helper.NumberOfItems.Config> option
      MaxNumberOfBooleanOperatorsInCondition : RuleConfig<Helper.NumberOfItems.Config> option
      FavourIgnoreOverLetWild : EnabledConfig option
      WildcardNamedWithAsPattern : EnabledConfig option
      UselessBinding : EnabledConfig option
      TupleOfWildcards : EnabledConfig option
      Indentation : EnabledConfig option
      MaxCharactersOnLine : RuleConfig<MaxCharactersOnLine.Config> option
      TrailingWhitespaceOnLine : RuleConfig<TrailingWhitespaceOnLine.Config> option
      MaxLinesInFile : RuleConfig<MaxLinesInFile.Config> option
      TrailingNewLineInFile : EnabledConfig option
      NoTabCharacters : EnabledConfig option }
with
    static member Zero = {
        Global = None
        ignoreFiles = None
        Hints = None
        // Configs for rules.
        TypedItemSpacing = None
        TypePrefixing = None
        UnionDefinitionIndentation = None
        ModuleDeclSpacing = None
        ClassMemberSpacing = None
        TupleCommaSpacing = None
        TupleIndentation = None
        TupleParentheses = None
        PatternMatchClausesOnNewLine = None
        PatternMatchOrClausesOnNewLine = None
        PatternMatchClauseIndentation = None
        PatternMatchExpressionIndentation = None
        RecursiveAsyncFunction = None
        RedundantNewKeyword = None
        NestedStatements = None
        ReimplementsFunction = None
        CanBeReplacedWithComposition = None
        FailwithWithSingleArgument = None
        RaiseWithSingleArgument = None
        NullArgWithSingleArgument = None
        InvalidOpWithSingleArgument = None
        InvalidArgWithTwoArguments = None
        FailwithfWithArgumentsMatchingFormatString = None
        MaxLinesInLambdaFunction = None
        MaxLinesInMatchLambdaFunction = None
        MaxLinesInValue = None
        MaxLinesInFunction = None
        MaxLinesInMember = None
        MaxLinesInConstructor = None
        MaxLinesInProperty = None
        MaxLinesInModule = None
        MaxLinesInRecord = None
        MaxLinesInEnum = None
        MaxLinesInUnion = None
        MaxLinesInClass = None
        InterfaceNames = None
        ExceptionNames = None
        TypeNames = None
        RecordFieldNames = None
        EnumCasesNames = None
        UnionCasesNames = None
        ModuleNames = None
        LiteralNames = None
        NamespaceNames = None
        MemberNames = None
        ParameterNames = None
        MeasureTypeNames = None
        ActivePatternNames = None
        PublicValuesNames = None
        NonPublicValuesNames = None
        MaxNumberOfItemsInTuple = None
        MaxNumberOfFunctionParameters = None
        MaxNumberOfMembers = None
        MaxNumberOfBooleanOperatorsInCondition = None
        FavourIgnoreOverLetWild = None
        WildcardNamedWithAsPattern = None
        UselessBinding = None
        TupleOfWildcards = None
        Indentation = None
        MaxCharactersOnLine = None
        TrailingWhitespaceOnLine = None
        MaxLinesInFile = None
        TrailingNewLineInFile = None
        NoTabCharacters = None
    }

// fsharplint:enable RecordFieldNames

/// Tries to parse the provided config text.
let internal parseConfig (configText:string) =
    try
        JsonConvert.DeserializeObject<Configuration>(configText, FSharpJsonConverter.serializerSettings)
    with
    | ex -> raise <| ConfigurationException(sprintf "Couldn't parse config, error=%s" ex.Message)

/// Tries to parse the config file at the provided path.
let internal loadConfig (configPath:string) =
    File.ReadAllText configPath
    |> parseConfig

/// A default configuration specifying every analyser and rule is included as a resource file in the framework.
/// This function loads and returns this default configuration.
let internal defaultConfiguration =
    let assembly = typeof<Rules.Rule>.GetTypeInfo().Assembly
    let resourceName = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                     |> Seq.find (fun n -> n.EndsWith("DefaultConfiguration.json", System.StringComparison.Ordinal))
    use stream = assembly.GetManifestResourceStream(resourceName)
    match stream with
    | null -> failwithf "Resource '%s' not found in assembly '%s'" resourceName (assembly.FullName)
    | stream ->
        use reader = new System.IO.StreamReader(stream)

        reader.ReadToEnd()
        |> parseConfig

type internal LineRules =
    { GenericLineRules : RuleMetadata<LineRuleConfig> []
      NoTabCharactersRule : RuleMetadata<NoTabCharactersRuleConfig> option
      IndentationRule : RuleMetadata<IndentationRuleConfig> option }

type internal LoadedRules =
    { GlobalConfig : Rules.GlobalRuleConfig
      AstNodeRules : RuleMetadata<AstNodeRuleConfig> []
      LineRules : LineRules }

let private getGlobalConfig (globalConfig:GlobalConfig option) =
    globalConfig
    |> Option.map (fun globalConfig -> {
        Rules.GlobalRuleConfig.numIndentationSpaces = globalConfig.numIndentationSpaces |> Option.defaultValue Rules.GlobalRuleConfig.Default.numIndentationSpaces
    }) |> Option.defaultValue Rules.GlobalRuleConfig.Default

let private parseHints (hints:string []) =
    let parseHint hint =
        match FParsec.CharParsers.run HintParser.phint hint with
        | FParsec.CharParsers.Success(hint, _, _) -> hint
        | FParsec.CharParsers.Failure(error, _, _) ->
            raise <| ConfigurationException("Failed to parse hint: " + hint + "\n" + error)

    hints
    |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Array.map parseHint
    |> Array.toList
    |> MergeSyntaxTrees.mergeHints

let internal flattenConfig (config:Configuration) =
    let allRules =
        [|
            config.TypedItemSpacing |> Option.bind (constructRuleWithConfig TypedItemSpacing.rule)
            config.TypePrefixing |> Option.bind (constructRuleIfEnabled TypePrefixing.rule)
            config.UnionDefinitionIndentation |> Option.bind (constructRuleIfEnabled UnionDefinitionIndentation.rule)
            config.ModuleDeclSpacing |> Option.bind (constructRuleIfEnabled ModuleDeclSpacing.rule)
            config.ClassMemberSpacing |> Option.bind (constructRuleIfEnabled ClassMemberSpacing.rule)
            config.TupleCommaSpacing |> Option.bind (constructRuleIfEnabled TupleCommaSpacing.rule)
            config.TupleIndentation |> Option.bind (constructRuleIfEnabled TupleIndentation.rule)
            config.TupleParentheses |> Option.bind (constructRuleIfEnabled TupleParentheses.rule)
            config.PatternMatchClausesOnNewLine |> Option.bind (constructRuleIfEnabled PatternMatchClausesOnNewLine.rule)
            config.PatternMatchOrClausesOnNewLine |> Option.bind (constructRuleIfEnabled PatternMatchOrClausesOnNewLine.rule)
            config.PatternMatchClauseIndentation |> Option.bind (constructRuleIfEnabled PatternMatchClauseIndentation.rule)
            config.PatternMatchExpressionIndentation |> Option.bind (constructRuleIfEnabled PatternMatchExpressionIndentation.rule)
            config.RecursiveAsyncFunction |> Option.bind (constructRuleIfEnabled RecursiveAsyncFunction.rule)
            config.RedundantNewKeyword |> Option.bind (constructRuleIfEnabled RedundantNewKeyword.rule)
            config.NestedStatements |> Option.bind (constructRuleWithConfig NestedStatements.rule)
            config.ReimplementsFunction |> Option.bind (constructRuleIfEnabled ReimplementsFunction.rule)
            config.CanBeReplacedWithComposition |> Option.bind (constructRuleIfEnabled CanBeReplacedWithComposition.rule)
            config.RaiseWithSingleArgument |> Option.bind (constructRuleIfEnabled RaiseWithSingleArgument.rule)
            config.FailwithWithSingleArgument |> Option.bind (constructRuleIfEnabled FailwithWithSingleArgument.rule)
            config.NullArgWithSingleArgument |> Option.bind (constructRuleIfEnabled NullArgWithSingleArgument.rule)
            config.InvalidOpWithSingleArgument |> Option.bind (constructRuleIfEnabled InvalidOpWithSingleArgument.rule)
            config.InvalidArgWithTwoArguments |> Option.bind (constructRuleIfEnabled InvalidArgWithTwoArguments.rule)
            config.FailwithfWithArgumentsMatchingFormatString |> Option.bind (constructRuleIfEnabled FailwithfWithArgumentsMatchingFormatString.rule)
            config.MaxLinesInLambdaFunction |> Option.bind (constructRuleWithConfig MaxLinesInLambdaFunction.rule)
            config.MaxLinesInMatchLambdaFunction |> Option.bind (constructRuleWithConfig MaxLinesInMatchLambdaFunction.rule)
            config.MaxLinesInValue |> Option.bind (constructRuleWithConfig MaxLinesInValue.rule)
            config.MaxLinesInFunction |> Option.bind (constructRuleWithConfig MaxLinesInFunction.rule)
            config.MaxLinesInMember |> Option.bind (constructRuleWithConfig MaxLinesInMember.rule)
            config.MaxLinesInConstructor |> Option.bind (constructRuleWithConfig MaxLinesInConstructor.rule)
            config.MaxLinesInProperty |> Option.bind (constructRuleWithConfig MaxLinesInProperty.rule)
            config.MaxLinesInModule |> Option.bind (constructRuleWithConfig MaxLinesInModule.rule)
            config.MaxLinesInRecord |> Option.bind (constructRuleWithConfig MaxLinesInRecord.rule)
            config.MaxLinesInEnum |> Option.bind (constructRuleWithConfig MaxLinesInEnum.rule)
            config.MaxLinesInUnion |> Option.bind (constructRuleWithConfig MaxLinesInUnion.rule)
            config.MaxLinesInClass |> Option.bind (constructRuleWithConfig MaxLinesInClass.rule)
            config.InterfaceNames |> Option.bind (constructRuleWithConfig InterfaceNames.rule)
            config.ExceptionNames |> Option.bind (constructRuleWithConfig ExceptionNames.rule)
            config.TypeNames |> Option.bind (constructRuleWithConfig TypeNames.rule)
            config.RecordFieldNames |> Option.bind (constructRuleWithConfig RecordFieldNames.rule)
            config.EnumCasesNames |> Option.bind (constructRuleWithConfig EnumCasesNames.rule)
            config.UnionCasesNames |> Option.bind (constructRuleWithConfig UnionCasesNames.rule)
            config.ModuleNames |> Option.bind (constructRuleWithConfig ModuleNames.rule)
            config.LiteralNames |> Option.bind (constructRuleWithConfig LiteralNames.rule)
            config.NamespaceNames |> Option.bind (constructRuleWithConfig NamespaceNames.rule)
            config.MemberNames |> Option.bind (constructRuleWithConfig MemberNames.rule)
            config.ParameterNames |> Option.bind (constructRuleWithConfig ParameterNames.rule)
            config.MeasureTypeNames |> Option.bind (constructRuleWithConfig MeasureTypeNames.rule)
            config.ActivePatternNames |> Option.bind (constructRuleWithConfig ActivePatternNames.rule)
            config.PublicValuesNames |> Option.bind (constructRuleWithConfig PublicValuesNames.rule)
            config.NonPublicValuesNames |> Option.bind (constructRuleWithConfig NonPublicValuesNames.rule)
            config.MaxNumberOfItemsInTuple |> Option.bind (constructRuleWithConfig MaxNumberOfItemsInTuple.rule)
            config.MaxNumberOfFunctionParameters |> Option.bind (constructRuleWithConfig MaxNumberOfFunctionParameters.rule)
            config.MaxNumberOfMembers |> Option.bind (constructRuleWithConfig MaxNumberOfMembers.rule)
            config.MaxNumberOfBooleanOperatorsInCondition |> Option.bind (constructRuleWithConfig MaxNumberOfBooleanOperatorsInCondition.rule)
            config.FavourIgnoreOverLetWild |> Option.bind (constructRuleIfEnabled FavourIgnoreOverLetWild.rule)
            config.WildcardNamedWithAsPattern |> Option.bind (constructRuleIfEnabled WildcardNamedWithAsPattern.rule)
            config.UselessBinding |> Option.bind (constructRuleIfEnabled UselessBinding.rule)
            config.TupleOfWildcards |> Option.bind (constructRuleIfEnabled TupleOfWildcards.rule)
            config.Indentation |> Option.bind (constructRuleIfEnabled Indentation.rule)
            config.MaxCharactersOnLine |> Option.bind (constructRuleWithConfig MaxCharactersOnLine.rule)
            config.TrailingWhitespaceOnLine |> Option.bind (constructRuleWithConfig TrailingWhitespaceOnLine.rule)
            config.MaxLinesInFile |> Option.bind (constructRuleWithConfig MaxLinesInFile.rule)
            config.TrailingNewLineInFile |> Option.bind (constructRuleIfEnabled TrailingNewLineInFile.rule)
            config.NoTabCharacters |> Option.bind (constructRuleIfEnabled NoTabCharacters.rule)
        |] |> Array.choose id

    let astNodeRules = ResizeArray()
    let lineRules = ResizeArray()
    let mutable indentationRule = None
    let mutable noTabCharactersRule = None
    allRules
    |> Array.distinctBy (function // Discard any deprecated rules which were define in a non-deprecated form.
        | Rule.AstNodeRule rule -> rule.Identifier
        | Rule.LineRule rule -> rule.Identifier
        | Rule.IndentationRule rule -> rule.Identifier
        | Rule.NoTabCharactersRule rule -> rule.Identifier)
    |> Array.iter (function
        | AstNodeRule rule -> astNodeRules.Add rule
        | LineRule rule -> lineRules.Add(rule)
        | IndentationRule rule -> indentationRule <- Some rule
        | NoTabCharactersRule rule -> noTabCharactersRule <- Some rule)

    { LoadedRules.GlobalConfig = getGlobalConfig config.Global
      AstNodeRules = astNodeRules.ToArray()
      LineRules =
          { GenericLineRules = lineRules.ToArray()
            IndentationRule = indentationRule
            NoTabCharactersRule = noTabCharactersRule } }
