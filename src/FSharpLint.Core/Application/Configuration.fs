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
let SettingsFileName = "fsharplint.json"

exception ConfigurationException of string

module FSharpJsonConverter =
    open Microsoft.FSharp.Reflection

    type OptionConverter() =
        inherit JsonConverter()

        override x.CanConvert(t) =
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

        override x.WriteJson(writer, value, serializer) =
            let value =
                if value = null then null
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
            if value = null then FSharpValue.MakeUnion(cases.[0], [||])
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

module IgnoreFiles =

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
    { maxNumberOfItemsInTuple : RuleConfig<Helper.NumberOfItems.Config> option
      maxNumberOfFunctionParameters : RuleConfig<Helper.NumberOfItems.Config> option
      maxNumberOfMembers : RuleConfig<Helper.NumberOfItems.Config> option
      maxNumberOfBooleanOperatorsInCondition : RuleConfig<Helper.NumberOfItems.Config> option }
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
      naming : NamesConfig option
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
            this.naming |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            this.numberOfItems |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            this.binding |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
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

let private getOrEmptyList hints = hints |> Option.defaultValue [||]

type HintConfig = {
    add : string [] option
    ignore : string [] option
}

type Configuration =
    { // Deprecated grouped configs. TODO: remove in next major release
      /// DEPRECATED, use IgnoreFiles.
      ignoreFiles : string [] option
      /// DEPRECATED, use Hints.
      hints : HintConfig option
      /// DEPRECATED, provide formatting rules at root level.
      formatting : FormattingConfig option
      /// DEPRECATED, provide conventions rules at root level.
      conventions : ConventionsConfig option
      /// DEPRECATED, provide typography rules at root level.
      typography : TypographyConfig option
      // Configs for rules.
      IgnoreFiles : string [] option
      Hints : string [] option
      TypedItemSpacing : RuleConfig<TypedItemSpacing.NewConfig> option
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
      NestedStatements : RuleConfig<NestedStatements.NewConfig> option
      ReimplementsFunction : EnabledConfig option
      CanBeReplacedWithComposition : EnabledConfig option
      RaiseWithSingleArgument : EnabledConfig option
      NullArgWithSingleArgument : EnabledConfig option
      InvalidOpWithSingleArgument : EnabledConfig option
      InvalidArgWithTwoArguments : EnabledConfig option
      FailwithfWithArgumentsMatchingFormatString : EnabledConfig option
      MaxLinesInLambdaFunction : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInMatchLambdaFunction : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInValue : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInFunction : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInMember : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInConstructor : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInProperty : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInModule : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInRecord : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInEnum : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInUnion : RuleConfig<Helper.SourceLength.NewConfig> option
      MaxLinesInClass : RuleConfig<Helper.SourceLength.NewConfig> option
      InterfaceNames : RuleConfig<NewNamingConfig> option
      ExceptionNames : RuleConfig<NewNamingConfig> option
      TypeNames : RuleConfig<NewNamingConfig> option
      RecordFieldNames : RuleConfig<NewNamingConfig> option
      EnumCasesNames : RuleConfig<NewNamingConfig> option
      UnionCasesNames : RuleConfig<NewNamingConfig> option
      ModuleNames : RuleConfig<NewNamingConfig> option
      LiteralNames : RuleConfig<NewNamingConfig> option
      NamespaceNames : RuleConfig<NewNamingConfig> option
      MemberNames : RuleConfig<NewNamingConfig> option
      ParameterNames : RuleConfig<NewNamingConfig> option
      MeasureTypeNames : RuleConfig<NewNamingConfig> option
      ActivePatternNames : RuleConfig<NewNamingConfig> option
      PublicValuesNames : RuleConfig<NewNamingConfig> option
      NonPublicValuesNames : RuleConfig<NewNamingConfig> option
      MaxNumberOfItemsInTuple : RuleConfig<Helper.NumberOfItems.NewConfig> option
      MaxNumberOfFunctionParameters : RuleConfig<Helper.NumberOfItems.NewConfig> option
      MaxNumberOfMembers : RuleConfig<Helper.NumberOfItems.NewConfig> option
      MaxNumberOfBooleanOperatorsInCondition : RuleConfig<Helper.NumberOfItems.NewConfig> option
      FavourIgnoreOverLetWild : EnabledConfig option
      WildcardNamedWithAsPattern : EnabledConfig option
      UselessBinding : EnabledConfig option
      TupleOfWildcards : EnabledConfig option
      Indentation : RuleConfig<Indentation.NewConfig> option
      MaxCharactersOnLine : RuleConfig<MaxCharactersOnLine.NewConfig> option
      TrailingWhitespaceOnLine : RuleConfig<TrailingWhitespaceOnLine.NewConfig> option
      MaxLinesInFile : RuleConfig<MaxLinesInFile.NewConfig> option
      TrailingNewLineInFile : EnabledConfig option
      NoTabCharacters : EnabledConfig option }
with
    static member Zero = {
        ignoreFiles = None
        hints = None
        formatting = None
        conventions = None
        typography = None
        // Configs for rules.
        IgnoreFiles = None
        Hints = None
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

/// Tries to parse the provided config text.
let parseConfig (configText : string) =
    try
        JsonConvert.DeserializeObject<Configuration>(configText, FSharpJsonConverter.serializerSettings)
    with
    | ex -> raise <| ConfigurationException(sprintf "Couldn't parse config, error=%s" ex.Message)

/// Tries to parse the config file at the provided path.
let loadConfig (configPath : string) =
    File.ReadAllText configPath
    |> parseConfig

/// A default configuration specifying every analyser and rule is included as a resource file in the framework.
/// This function loads and returns this default configuration.
let defaultConfiguration =
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

let serializeConfig (config : Configuration) =
    JsonConvert.SerializeObject(config, FSharpJsonConverter.serializerSettings)

type LineRules =
    { genericLineRules : RuleMetadata<LineRuleConfig> []
      noTabCharactersRule : RuleMetadata<NoTabCharactersRuleConfig> option
      indentationRule : RuleMetadata<IndentationRuleConfig> option }

type LoadedRules =
    { astNodeRules : RuleMetadata<AstNodeRuleConfig> []
      lineRules : LineRules
      deprecatedRules : Rule [] }

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

let flattenConfig (config : Configuration) =
    let deprecatedAllRules =
        [|
            config.formatting |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.conventions |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.typography |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.hints |> Option.map (fun config -> HintMatcher.rule { HintMatcher.Config.hintTrie = parseHints (getOrEmptyList config.add) }) |> Option.toArray
        |] |> Array.concat

    let allRules =
        [|
            config.TypedItemSpacing |> Option.bind (constructRuleWithConfig TypedItemSpacing.newRule)
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
            config.NestedStatements |> Option.bind (constructRuleWithConfig NestedStatements.newRule)
            config.ReimplementsFunction |> Option.bind (constructRuleIfEnabled ReimplementsFunction.rule)
            config.CanBeReplacedWithComposition |> Option.bind (constructRuleIfEnabled CanBeReplacedWithComposition.rule)
            config.RaiseWithSingleArgument |> Option.bind (constructRuleIfEnabled RaiseWithSingleArgument.rule)
            config.NullArgWithSingleArgument |> Option.bind (constructRuleIfEnabled NullArgWithSingleArgument.rule)
            config.InvalidOpWithSingleArgument |> Option.bind (constructRuleIfEnabled InvalidOpWithSingleArgument.rule)
            config.InvalidArgWithTwoArguments |> Option.bind (constructRuleIfEnabled InvalidArgWithTwoArguments.rule)
            config.FailwithfWithArgumentsMatchingFormatString |> Option.bind (constructRuleIfEnabled FailwithfWithArgumentsMatchingFormatString.rule)
            config.MaxLinesInLambdaFunction |> Option.bind (constructRuleWithConfig MaxLinesInLambdaFunction.newRule)
            config.MaxLinesInMatchLambdaFunction |> Option.bind (constructRuleWithConfig MaxLinesInMatchLambdaFunction.newRule)
            config.MaxLinesInValue |> Option.bind (constructRuleWithConfig MaxLinesInValue.newRule)
            config.MaxLinesInFunction |> Option.bind (constructRuleWithConfig MaxLinesInFunction.newRule)
            config.MaxLinesInMember |> Option.bind (constructRuleWithConfig MaxLinesInMember.newRule)
            config.MaxLinesInConstructor |> Option.bind (constructRuleWithConfig MaxLinesInConstructor.newRule)
            config.MaxLinesInProperty |> Option.bind (constructRuleWithConfig MaxLinesInProperty.newRule)
            config.MaxLinesInModule |> Option.bind (constructRuleWithConfig MaxLinesInModule.newRule)
            config.MaxLinesInRecord |> Option.bind (constructRuleWithConfig MaxLinesInRecord.newRule)
            config.MaxLinesInEnum |> Option.bind (constructRuleWithConfig MaxLinesInEnum.newRule)
            config.MaxLinesInUnion |> Option.bind (constructRuleWithConfig MaxLinesInUnion.newRule)
            config.MaxLinesInClass |> Option.bind (constructRuleWithConfig MaxLinesInClass.newRule)
            config.InterfaceNames |> Option.bind (constructRuleWithConfig InterfaceNames.newRule)
            config.ExceptionNames |> Option.bind (constructRuleWithConfig ExceptionNames.newRule)
            config.TypeNames |> Option.bind (constructRuleWithConfig TypeNames.newRule)
            config.RecordFieldNames |> Option.bind (constructRuleWithConfig RecordFieldNames.newRule)
            config.EnumCasesNames |> Option.bind (constructRuleWithConfig EnumCasesNames.newRule)
            config.UnionCasesNames |> Option.bind (constructRuleWithConfig UnionCasesNames.newRule)
            config.ModuleNames |> Option.bind (constructRuleWithConfig ModuleNames.newRule)
            config.LiteralNames |> Option.bind (constructRuleWithConfig LiteralNames.newRule)
            config.NamespaceNames |> Option.bind (constructRuleWithConfig NamespaceNames.newRule)
            config.MemberNames |> Option.bind (constructRuleWithConfig MemberNames.newRule)
            config.ParameterNames |> Option.bind (constructRuleWithConfig ParameterNames.newRule)
            config.MeasureTypeNames |> Option.bind (constructRuleWithConfig MeasureTypeNames.newRule)
            config.ActivePatternNames |> Option.bind (constructRuleWithConfig ActivePatternNames.newRule)
            config.PublicValuesNames |> Option.bind (constructRuleWithConfig PublicValuesNames.newRule)
            config.NonPublicValuesNames |> Option.bind (constructRuleWithConfig NonPublicValuesNames.newRule)
            config.MaxNumberOfItemsInTuple |> Option.bind (constructRuleWithConfig MaxNumberOfItemsInTuple.newRule)
            config.MaxNumberOfFunctionParameters |> Option.bind (constructRuleWithConfig MaxNumberOfFunctionParameters.newRule)
            config.MaxNumberOfMembers |> Option.bind (constructRuleWithConfig MaxNumberOfMembers.newRule)
            config.MaxNumberOfBooleanOperatorsInCondition |> Option.bind (constructRuleWithConfig MaxNumberOfBooleanOperatorsInCondition.newRule)
            config.FavourIgnoreOverLetWild |> Option.bind (constructRuleIfEnabled FavourIgnoreOverLetWild.rule)
            config.WildcardNamedWithAsPattern |> Option.bind (constructRuleIfEnabled WildcardNamedWithAsPattern.rule)
            config.UselessBinding |> Option.bind (constructRuleIfEnabled UselessBinding.rule)
            config.TupleOfWildcards |> Option.bind (constructRuleIfEnabled TupleOfWildcards.rule)
            config.Indentation |> Option.bind (constructRuleWithConfig Indentation.newRule)
            config.MaxCharactersOnLine |> Option.bind (constructRuleWithConfig MaxCharactersOnLine.newRule)
            config.TrailingWhitespaceOnLine |> Option.bind (constructRuleWithConfig TrailingWhitespaceOnLine.newRule)
            config.MaxLinesInFile |> Option.bind (constructRuleWithConfig MaxLinesInFile.newRule)
            config.TrailingNewLineInFile |> Option.bind (constructRuleIfEnabled TrailingNewLineInFile.rule)
            config.NoTabCharacters |> Option.bind (constructRuleIfEnabled NoTabCharacters.rule)
        |] |> Array.choose id

    let astNodeRules = ResizeArray()
    let lineRules = ResizeArray()
    let mutable indentationRule = None
    let mutable noTabCharactersRule = None
    Array.append allRules deprecatedAllRules
    |> Array.distinctBy (function // Discard any deprecated rules which were define in a non-deprecated form.
        | Rule.AstNodeRule rule -> rule.identifier
        | Rule.LineRule rule -> rule.identifier
        | Rule.IndentationRule rule -> rule.identifier
        | Rule.NoTabCharactersRule rule -> rule.identifier)
    |> Array.iter (function
        | AstNodeRule rule -> astNodeRules.Add rule
        | LineRule rule -> lineRules.Add(rule)
        | IndentationRule rule -> indentationRule <- Some rule
        | NoTabCharactersRule rule -> noTabCharactersRule <- Some rule)

    { LoadedRules.astNodeRules = astNodeRules.ToArray()
      lineRules =
          { genericLineRules = lineRules.ToArray()
            indentationRule = indentationRule
            noTabCharactersRule = noTabCharactersRule }
      deprecatedRules = deprecatedAllRules }
