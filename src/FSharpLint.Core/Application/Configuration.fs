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

// Non-standard record field naming for config serialization.
// fsharplint:disable RecordFieldNames
type RuleConfig<'Config> = {
    Enabled:bool
    Config:'Config option
}

type EnabledConfig = RuleConfig<unit>

let constructRuleIfEnabled rule ruleConfig = if ruleConfig.Enabled then Some rule else None

let constructRuleWithConfig rule ruleConfig =
    if ruleConfig.Enabled then
        ruleConfig.Config |> Option.map rule
    else
        None

type TupleFormattingConfig =
    { tupleCommaSpacing:EnabledConfig option
      tupleIndentation:EnabledConfig option
      tupleParentheses:EnabledConfig option }
with
    member this.Flatten() =
        [|
            this.tupleCommaSpacing |> Option.bind (constructRuleIfEnabled TupleCommaSpacing.rule)
            this.tupleIndentation |> Option.bind (constructRuleIfEnabled TupleIndentation.rule)
            this.tupleParentheses |> Option.bind (constructRuleIfEnabled TupleParentheses.rule)
        |] |> Array.choose id

type PatternMatchFormattingConfig =
    { patternMatchClausesOnNewLine:EnabledConfig option
      patternMatchOrClausesOnNewLine:EnabledConfig option
      patternMatchClauseIndentation:RuleConfig<PatternMatchClauseIndentation.Config> option
      patternMatchExpressionIndentation:EnabledConfig option }
with
    member this.Flatten() =
        [|
            this.patternMatchClausesOnNewLine |> Option.bind (constructRuleIfEnabled PatternMatchClausesOnNewLine.rule)
            this.patternMatchOrClausesOnNewLine |> Option.bind (constructRuleIfEnabled PatternMatchOrClausesOnNewLine.rule)
            this.patternMatchClauseIndentation |> Option.bind (constructRuleWithConfig PatternMatchClauseIndentation.rule)
            this.patternMatchExpressionIndentation |> Option.bind (constructRuleIfEnabled PatternMatchExpressionIndentation.rule)
        |] |> Array.choose id

type FormattingConfig =
    { typedItemSpacing:RuleConfig<TypedItemSpacing.Config> option
      typePrefixing:EnabledConfig option
      unionDefinitionIndentation:EnabledConfig option
      moduleDeclSpacing:EnabledConfig option
      classMemberSpacing:EnabledConfig option
      tupleFormatting:TupleFormattingConfig option
      patternMatchFormatting:PatternMatchFormattingConfig option }
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
    { failwithBadUsage:EnabledConfig option
      raiseWithSingleArgument:EnabledConfig option
      nullArgWithSingleArgument:EnabledConfig option
      invalidOpWithSingleArgument:EnabledConfig option
      invalidArgWithTwoArguments:EnabledConfig option
      failwithfWithArgumentsMatchingFormatString:EnabledConfig option }
with
    member this.Flatten() =
        [|
            this.failwithBadUsage |> Option.bind (constructRuleIfEnabled FailwithBadUsage.rule) |> Option.toArray
            this.raiseWithSingleArgument |> Option.bind (constructRuleIfEnabled RaiseWithSingleArgument.rule) |> Option.toArray
            this.nullArgWithSingleArgument |> Option.bind (constructRuleIfEnabled NullArgWithSingleArgument.rule) |> Option.toArray
            this.invalidOpWithSingleArgument |> Option.bind (constructRuleIfEnabled InvalidOpWithSingleArgument.rule) |> Option.toArray
            this.invalidArgWithTwoArguments |> Option.bind (constructRuleIfEnabled InvalidArgWithTwoArguments.rule) |> Option.toArray
            this.failwithfWithArgumentsMatchingFormatString |> Option.bind (constructRuleIfEnabled FailwithfWithArgumentsMatchingFormatString.rule) |> Option.toArray
        |] |> Array.concat

type SourceLengthConfig =
    { maxLinesInLambdaFunction:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInMatchLambdaFunction:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInValue:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInFunction:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInMember:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInConstructor:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInProperty:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInModule:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInRecord:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInEnum:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInUnion:RuleConfig<Helper.SourceLength.Config> option
      maxLinesInClass:RuleConfig<Helper.SourceLength.Config> option }
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
    { interfaceNames:RuleConfig<NamingConfig> option
      genericTypesNames:RuleConfig<NamingConfig> option
      exceptionNames:RuleConfig<NamingConfig> option
      typeNames:RuleConfig<NamingConfig> option
      recordFieldNames:RuleConfig<NamingConfig> option
      enumCasesNames:RuleConfig<NamingConfig> option
      unionCasesNames:RuleConfig<NamingConfig> option
      moduleNames:RuleConfig<NamingConfig> option
      literalNames:RuleConfig<NamingConfig> option
      namespaceNames:RuleConfig<NamingConfig> option
      memberNames:RuleConfig<NamingConfig> option
      parameterNames:RuleConfig<NamingConfig> option
      measureTypeNames:RuleConfig<NamingConfig> option
      activePatternNames:RuleConfig<NamingConfig> option
      publicValuesNames:RuleConfig<NamingConfig> option
      nonPublicValuesNames:RuleConfig<NamingConfig> option
      privateValuesNames:RuleConfig<NamingConfig> option
      internalValuesNames:RuleConfig<NamingConfig> option }
with
    member this.Flatten() =
        [|
            this.interfaceNames |> Option.bind (constructRuleWithConfig InterfaceNames.rule) |> Option.toArray
            this.genericTypesNames |> Option.bind (constructRuleWithConfig GenericTypesNames.rule) |> Option.toArray
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
            this.nonPublicValuesNames |> Option.bind (constructRuleWithConfig PrivateValuesNames.rule) |> Option.toArray
            this.nonPublicValuesNames |> Option.bind (constructRuleWithConfig InternalValuesNames.rule) |> Option.toArray
            this.privateValuesNames |> Option.bind (constructRuleWithConfig PrivateValuesNames.rule) |> Option.toArray
            this.internalValuesNames|> Option.bind (constructRuleWithConfig InternalValuesNames.rule) |> Option.toArray
        |] |> Array.concat

type NumberOfItemsConfig =
    { maxNumberOfItemsInTuple:RuleConfig<Helper.NumberOfItems.Config> option
      maxNumberOfFunctionParameters:RuleConfig<Helper.NumberOfItems.Config> option
      maxNumberOfMembers:RuleConfig<Helper.NumberOfItems.Config> option
      maxNumberOfBooleanOperatorsInCondition:RuleConfig<Helper.NumberOfItems.Config> option }
with
    member this.Flatten() =
        [|
            this.maxNumberOfItemsInTuple |> Option.bind (constructRuleWithConfig MaxNumberOfItemsInTuple.rule) |> Option.toArray
            this.maxNumberOfFunctionParameters |> Option.bind (constructRuleWithConfig MaxNumberOfFunctionParameters.rule) |> Option.toArray
            this.maxNumberOfMembers |> Option.bind (constructRuleWithConfig MaxNumberOfMembers.rule) |> Option.toArray
            this.maxNumberOfBooleanOperatorsInCondition |> Option.bind (constructRuleWithConfig MaxNumberOfBooleanOperatorsInCondition.rule) |> Option.toArray
        |] |> Array.concat

type BindingConfig =
    { favourIgnoreOverLetWild:EnabledConfig option
      wildcardNamedWithAsPattern:EnabledConfig option
      uselessBinding:EnabledConfig option
      tupleOfWildcards:EnabledConfig option
      favourTypedIgnore:EnabledConfig option }
with
    member this.Flatten() =
        [|
            this.favourIgnoreOverLetWild |> Option.bind (constructRuleIfEnabled FavourIgnoreOverLetWild.rule) |> Option.toArray
            this.favourTypedIgnore |> Option.bind (constructRuleIfEnabled FavourTypedIgnore.rule) |> Option.toArray
            this.wildcardNamedWithAsPattern |> Option.bind (constructRuleIfEnabled WildcardNamedWithAsPattern.rule) |> Option.toArray
            this.uselessBinding |> Option.bind (constructRuleIfEnabled UselessBinding.rule) |> Option.toArray
            this.tupleOfWildcards |> Option.bind (constructRuleIfEnabled TupleOfWildcards.rule) |> Option.toArray
        |] |> Array.concat

type ConventionsConfig =
    { recursiveAsyncFunction:EnabledConfig option
      avoidTooShortNames:EnabledConfig option
      redundantNewKeyword:EnabledConfig option
      favourStaticEmptyFields:EnabledConfig option
      asyncExceptionWithoutReturn:EnabledConfig option
      nestedStatements:RuleConfig<NestedStatements.Config> option
      cyclomaticComplexity:RuleConfig<CyclomaticComplexity.Config> option
      reimplementsFunction:EnabledConfig option
      canBeReplacedWithComposition:EnabledConfig option
      avoidSinglePipeOperator:EnabledConfig option
      raiseWithTooManyArgs:RaiseWithTooManyArgsConfig option
      sourceLength:SourceLengthConfig option
      naming:NamesConfig option
      numberOfItems:NumberOfItemsConfig option
      binding:BindingConfig option
      favourReRaise:EnabledConfig option
      favourConsistentThis:RuleConfig<FavourConsistentThis.Config> option }
with
    member this.Flatten() =
        [|
            this.recursiveAsyncFunction |> Option.bind (constructRuleIfEnabled RecursiveAsyncFunction.rule) |> Option.toArray
            this.avoidTooShortNames |> Option.bind (constructRuleIfEnabled AvoidTooShortNames.rule) |> Option.toArray           
            this.redundantNewKeyword |> Option.bind (constructRuleIfEnabled RedundantNewKeyword.rule) |> Option.toArray
            this.favourReRaise |> Option.bind (constructRuleIfEnabled FavourReRaise.rule) |> Option.toArray
            this.favourStaticEmptyFields |> Option.bind (constructRuleIfEnabled FavourStaticEmptyFields.rule) |> Option.toArray
            this.asyncExceptionWithoutReturn |> Option.bind (constructRuleIfEnabled AsyncExceptionWithoutReturn.rule) |> Option.toArray
            this.nestedStatements |> Option.bind (constructRuleWithConfig NestedStatements.rule) |> Option.toArray
            this.favourConsistentThis |> Option.bind (constructRuleWithConfig FavourConsistentThis.rule) |> Option.toArray
            this.cyclomaticComplexity |> Option.bind (constructRuleWithConfig CyclomaticComplexity.rule) |> Option.toArray
            this.reimplementsFunction |> Option.bind (constructRuleIfEnabled ReimplementsFunction.rule) |> Option.toArray
            this.canBeReplacedWithComposition |> Option.bind (constructRuleIfEnabled CanBeReplacedWithComposition.rule) |> Option.toArray
            this.avoidSinglePipeOperator|> Option.bind (constructRuleIfEnabled AvoidSinglePipeOperator.rule) |> Option.toArray
            this.raiseWithTooManyArgs |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            this.sourceLength |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            this.naming |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            this.numberOfItems |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            this.binding |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
        |] |> Array.concat

type TypographyConfig =
    { indentation:EnabledConfig option
      maxCharactersOnLine:RuleConfig<MaxCharactersOnLine.Config> option
      trailingWhitespaceOnLine:RuleConfig<TrailingWhitespaceOnLine.Config> option
      maxLinesInFile:RuleConfig<MaxLinesInFile.Config> option
      trailingNewLineInFile:EnabledConfig option
      noTabCharacters:EnabledConfig option }
with
    member this.Flatten() =
        [|
            this.indentation |> Option.bind (constructRuleIfEnabled Indentation.rule) |> Option.toArray
            this.maxCharactersOnLine |> Option.bind (constructRuleWithConfig MaxCharactersOnLine.rule) |> Option.toArray
            this.trailingWhitespaceOnLine |> Option.bind (constructRuleWithConfig TrailingWhitespaceOnLine.rule) |> Option.toArray
            this.maxLinesInFile |> Option.bind (constructRuleWithConfig MaxLinesInFile.rule) |> Option.toArray
            this.trailingNewLineInFile |> Option.bind (constructRuleIfEnabled TrailingNewLineInFile.rule) |> Option.toArray
            this.noTabCharacters |> Option.bind (constructRuleIfEnabled NoTabCharacters.rule) |> Option.toArray
        |] |> Array.concat

let private getOrEmptyList hints = hints |> Option.defaultValue [||]

type HintConfig = {
    add:string [] option
    ignore:string [] option
}

type GlobalConfig = {
    numIndentationSpaces:int option
}

type Configuration =
    { Global:GlobalConfig option
      // Deprecated grouped configs. TODO: remove in next major release
      /// DEPRECATED, provide formatting rules at root level.
      formatting:FormattingConfig option
      /// DEPRECATED, provide conventions rules at root level.
      conventions:ConventionsConfig option
      /// DEPRECATED, provide typography rules at root level.
      typography:TypographyConfig option
      ignoreFiles:string [] option
      Hints:HintConfig option
      TypedItemSpacing:RuleConfig<TypedItemSpacing.Config> option
      TypePrefixing:EnabledConfig option
      UnionDefinitionIndentation:EnabledConfig option
      ModuleDeclSpacing:EnabledConfig option
      ClassMemberSpacing:EnabledConfig option
      TupleCommaSpacing:EnabledConfig option
      TupleIndentation:EnabledConfig option
      TupleParentheses:EnabledConfig option
      PatternMatchClausesOnNewLine:EnabledConfig option
      PatternMatchOrClausesOnNewLine:EnabledConfig option
      PatternMatchClauseIndentation:RuleConfig<PatternMatchClauseIndentation.Config> option
      PatternMatchExpressionIndentation:EnabledConfig option
      RecursiveAsyncFunction:EnabledConfig option
      AvoidTooShortNames:EnabledConfig option
      RedundantNewKeyword:EnabledConfig option
      FavourReRaise:EnabledConfig option
      FavourStaticEmptyFields:EnabledConfig option
      AsyncExceptionWithoutReturn:EnabledConfig option
      NestedStatements:RuleConfig<NestedStatements.Config> option
      FavourConsistentThis:RuleConfig<FavourConsistentThis.Config> option
      CyclomaticComplexity:RuleConfig<CyclomaticComplexity.Config> option
      ReimplementsFunction:EnabledConfig option
      CanBeReplacedWithComposition:EnabledConfig option
      AvoidSinglePipeOperator:EnabledConfig option
      FailwithBadUsage:EnabledConfig option
      RaiseWithSingleArgument:EnabledConfig option
      FailwithWithSingleArgument:EnabledConfig option
      NullArgWithSingleArgument:EnabledConfig option
      InvalidOpWithSingleArgument:EnabledConfig option
      InvalidArgWithTwoArguments:EnabledConfig option
      FailwithfWithArgumentsMatchingFormatString:EnabledConfig option
      MaxLinesInLambdaFunction:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInMatchLambdaFunction:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInValue:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInFunction:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInMember:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInConstructor:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInProperty:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInModule:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInRecord:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInEnum:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInUnion:RuleConfig<Helper.SourceLength.Config> option
      MaxLinesInClass:RuleConfig<Helper.SourceLength.Config> option
      InterfaceNames:RuleConfig<NamingConfig> option
      GenericTypesNames:RuleConfig<NamingConfig> option
      ExceptionNames:RuleConfig<NamingConfig> option
      TypeNames:RuleConfig<NamingConfig> option
      RecordFieldNames:RuleConfig<NamingConfig> option
      EnumCasesNames:RuleConfig<NamingConfig> option
      UnionCasesNames:RuleConfig<NamingConfig> option
      ModuleNames:RuleConfig<NamingConfig> option
      LiteralNames:RuleConfig<NamingConfig> option
      NamespaceNames:RuleConfig<NamingConfig> option
      MemberNames:RuleConfig<NamingConfig> option
      ParameterNames:RuleConfig<NamingConfig> option
      MeasureTypeNames:RuleConfig<NamingConfig> option
      ActivePatternNames:RuleConfig<NamingConfig> option
      PublicValuesNames:RuleConfig<NamingConfig> option
      NonPublicValuesNames:RuleConfig<NamingConfig> option
      PrivateValuesNames:RuleConfig<NamingConfig> option
      InternalValuesNames:RuleConfig<NamingConfig> option
      MaxNumberOfItemsInTuple:RuleConfig<Helper.NumberOfItems.Config> option
      MaxNumberOfFunctionParameters:RuleConfig<Helper.NumberOfItems.Config> option
      MaxNumberOfMembers:RuleConfig<Helper.NumberOfItems.Config> option
      MaxNumberOfBooleanOperatorsInCondition:RuleConfig<Helper.NumberOfItems.Config> option
      FavourIgnoreOverLetWild:EnabledConfig option
      FavourTypedIgnore:EnabledConfig option
      WildcardNamedWithAsPattern:EnabledConfig option
      UselessBinding:EnabledConfig option
      TupleOfWildcards:EnabledConfig option
      Indentation:EnabledConfig option
      MaxCharactersOnLine:RuleConfig<MaxCharactersOnLine.Config> option
      TrailingWhitespaceOnLine:RuleConfig<TrailingWhitespaceOnLine.Config> option
      MaxLinesInFile:RuleConfig<MaxLinesInFile.Config> option
      TrailingNewLineInFile:EnabledConfig option
      NoTabCharacters:EnabledConfig option
      NoPartialFunctions:RuleConfig<NoPartialFunctions.Config> option }
with
    static member Zero = {
        Global = None
        ignoreFiles = None
        Hints = None
        formatting = None
        conventions = None
        typography = None
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
        AvoidTooShortNames = None
        RedundantNewKeyword = None
        FavourReRaise = None
        FavourStaticEmptyFields = None
        AsyncExceptionWithoutReturn = None
        NestedStatements = None
        FavourConsistentThis = None
        CyclomaticComplexity = None
        ReimplementsFunction = None
        CanBeReplacedWithComposition = None
        AvoidSinglePipeOperator = None
        FailwithWithSingleArgument = None
        FailwithBadUsage = None
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
        GenericTypesNames = None
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
        PrivateValuesNames = None
        InternalValuesNames = None
        MaxNumberOfItemsInTuple = None
        MaxNumberOfFunctionParameters = None
        MaxNumberOfMembers = None
        MaxNumberOfBooleanOperatorsInCondition = None
        FavourIgnoreOverLetWild = None
        FavourTypedIgnore = None
        WildcardNamedWithAsPattern = None
        UselessBinding = None
        TupleOfWildcards = None
        Indentation = None
        MaxCharactersOnLine = None
        TrailingWhitespaceOnLine = None
        MaxLinesInFile = None
        TrailingNewLineInFile = None
        NoTabCharacters = None
        NoPartialFunctions = None
    }

// fsharplint:enable RecordFieldNames

/// Tries to parse the provided config text.
let parseConfig (configText:string) =
    try
        JsonConvert.DeserializeObject<Configuration>(configText, FSharpJsonConverter.serializerSettings)
    with
    | ex -> raise <| ConfigurationException(sprintf "Couldn't parse config, error=%s" ex.Message)

/// Tries to parse the config file at the provided path.
let loadConfig (configPath:string) =
    File.ReadAllText configPath
    |> parseConfig

/// A default configuration specifying every analyser and rule is included as a resource file in the framework.
/// This function loads and returns this default configuration.
let defaultConfiguration =
    let assembly = typeof<Rules.Rule>.GetTypeInfo().Assembly
    let resourceName = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                       |> Seq.find (fun n -> n.EndsWith("fsharplint.json", System.StringComparison.Ordinal))
    use stream = assembly.GetManifestResourceStream(resourceName)
    match stream with
    | null -> failwithf "Resource '%s' not found in assembly '%s'" resourceName (assembly.FullName)
    | stream ->
        use reader = new System.IO.StreamReader(stream)

        reader.ReadToEnd()
        |> parseConfig

type LineRules =
    { GenericLineRules:RuleMetadata<LineRuleConfig> []
      NoTabCharactersRule:RuleMetadata<NoTabCharactersRuleConfig> option
      IndentationRule:RuleMetadata<IndentationRuleConfig> option }

type LoadedRules =
    { GlobalConfig:Rules.GlobalRuleConfig
      AstNodeRules:RuleMetadata<AstNodeRuleConfig> []
      LineRules:LineRules
      DeprecatedRules:Rule [] }

let getGlobalConfig (globalConfig:GlobalConfig option) =
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

let flattenConfig (config:Configuration) =
    let deprecatedAllRules =
        [|
            config.formatting |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.conventions |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.typography |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.Hints |> Option.map (fun config -> HintMatcher.rule { HintMatcher.Config.HintTrie = parseHints (getOrEmptyList config.add) }) |> Option.toArray
        |] |> Array.concat

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
            config.PatternMatchClauseIndentation |> Option.bind (constructRuleWithConfig PatternMatchClauseIndentation.rule)
            config.PatternMatchExpressionIndentation |> Option.bind (constructRuleIfEnabled PatternMatchExpressionIndentation.rule)
            config.RecursiveAsyncFunction |> Option.bind (constructRuleIfEnabled RecursiveAsyncFunction.rule)
            config.AvoidTooShortNames |> Option.bind (constructRuleIfEnabled AvoidTooShortNames.rule)
            config.RedundantNewKeyword |> Option.bind (constructRuleIfEnabled RedundantNewKeyword.rule)
            config.FavourReRaise |> Option.bind (constructRuleIfEnabled FavourReRaise.rule)
            config.FavourStaticEmptyFields |> Option.bind (constructRuleIfEnabled FavourStaticEmptyFields.rule)
            config.AsyncExceptionWithoutReturn |> Option.bind (constructRuleIfEnabled AsyncExceptionWithoutReturn.rule)
            config.NestedStatements |> Option.bind (constructRuleWithConfig NestedStatements.rule)
            config.FavourConsistentThis |> Option.bind (constructRuleWithConfig FavourConsistentThis.rule)
            config.CyclomaticComplexity |> Option.bind (constructRuleWithConfig CyclomaticComplexity.rule)
            config.ReimplementsFunction |> Option.bind (constructRuleIfEnabled ReimplementsFunction.rule)
            config.CanBeReplacedWithComposition |> Option.bind (constructRuleIfEnabled CanBeReplacedWithComposition.rule)
            config.AvoidSinglePipeOperator |> Option.bind (constructRuleIfEnabled AvoidSinglePipeOperator.rule)
            config.FailwithBadUsage |> Option.bind (constructRuleIfEnabled FailwithBadUsage.rule)
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
            config.GenericTypesNames |> Option.bind (constructRuleWithConfig GenericTypesNames.rule)
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
            config.NonPublicValuesNames |> Option.bind (constructRuleWithConfig PrivateValuesNames.rule)
            config.NonPublicValuesNames |> Option.bind (constructRuleWithConfig InternalValuesNames.rule)
            config.PrivateValuesNames |> Option.bind (constructRuleWithConfig PrivateValuesNames.rule)
            config.InternalValuesNames |> Option.bind (constructRuleWithConfig InternalValuesNames.rule)
            config.MaxNumberOfItemsInTuple |> Option.bind (constructRuleWithConfig MaxNumberOfItemsInTuple.rule)
            config.MaxNumberOfFunctionParameters |> Option.bind (constructRuleWithConfig MaxNumberOfFunctionParameters.rule)
            config.MaxNumberOfMembers |> Option.bind (constructRuleWithConfig MaxNumberOfMembers.rule)
            config.MaxNumberOfBooleanOperatorsInCondition |> Option.bind (constructRuleWithConfig MaxNumberOfBooleanOperatorsInCondition.rule)
            config.FavourIgnoreOverLetWild |> Option.bind (constructRuleIfEnabled FavourIgnoreOverLetWild.rule)
            config.FavourTypedIgnore |> Option.bind (constructRuleIfEnabled FavourTypedIgnore.rule)
            config.WildcardNamedWithAsPattern |> Option.bind (constructRuleIfEnabled WildcardNamedWithAsPattern.rule)
            config.UselessBinding |> Option.bind (constructRuleIfEnabled UselessBinding.rule)
            config.TupleOfWildcards |> Option.bind (constructRuleIfEnabled TupleOfWildcards.rule)
            config.Indentation |> Option.bind (constructRuleIfEnabled Indentation.rule)
            config.MaxCharactersOnLine |> Option.bind (constructRuleWithConfig MaxCharactersOnLine.rule)
            config.TrailingWhitespaceOnLine |> Option.bind (constructRuleWithConfig TrailingWhitespaceOnLine.rule)
            config.MaxLinesInFile |> Option.bind (constructRuleWithConfig MaxLinesInFile.rule)
            config.TrailingNewLineInFile |> Option.bind (constructRuleIfEnabled TrailingNewLineInFile.rule)
            config.NoTabCharacters |> Option.bind (constructRuleIfEnabled NoTabCharacters.rule)
            config.NoPartialFunctions |> Option.bind (constructRuleWithConfig NoPartialFunctions.rule)
        |] |> Array.choose id

    if config.NonPublicValuesNames.IsSome &&
        (config.PrivateValuesNames.IsSome || config.InternalValuesNames.IsSome) then
        failwith "nonPublicValuesNames has been deprecated, use privateValuesNames and/or internalValuesNames instead"

    let astNodeRules = ResizeArray()
    let lineRules = ResizeArray()
    let mutable indentationRule = None
    let mutable noTabCharactersRule = None
    Array.append allRules deprecatedAllRules
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
      DeprecatedRules = deprecatedAllRules
      AstNodeRules = astNodeRules.ToArray()
      LineRules =
        { GenericLineRules = lineRules.ToArray()
          IndentationRule = indentationRule
          NoTabCharactersRule = noTabCharactersRule } }
