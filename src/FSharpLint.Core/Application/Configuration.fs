/// Loads configuration file from JSON into an object.
module FSharpLint.Framework.Configuration

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
    open System
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

    open System
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

let tryOverride currentConfig overridingConfig overrideFunction =
    match currentConfig with
    | Some currentConfig ->
        match overridingConfig with
        | Some overridingConfig ->
            overrideFunction currentConfig overridingConfig |> Some
        | None ->
            Some currentConfig
    | None ->
        overridingConfig

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

    member this.Override(overriding:TupleFormattingConfig) =
        {
            TupleFormattingConfig.tupleCommaSpacing = overriding.tupleCommaSpacing |> Option.orElse this.tupleCommaSpacing
            tupleIndentation = overriding.tupleIndentation |> Option.orElse this.tupleIndentation
            tupleParentheses = overriding.tupleParentheses |> Option.orElse this.tupleParentheses
        }

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

    member this.Override(overriding:PatternMatchFormattingConfig) =
        {
            PatternMatchFormattingConfig.patternMatchClausesOnNewLine = overriding.patternMatchClausesOnNewLine |> Option.orElse this.patternMatchClausesOnNewLine
            patternMatchOrClausesOnNewLine = overriding.patternMatchOrClausesOnNewLine |> Option.orElse this.patternMatchOrClausesOnNewLine
            patternMatchClauseIndentation = overriding.patternMatchClauseIndentation |> Option.orElse this.patternMatchClauseIndentation
            patternMatchExpressionIndentation = overriding.patternMatchExpressionIndentation |> Option.orElse this.patternMatchExpressionIndentation
        }

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

    member this.Override(overriding:FormattingConfig) =
        {
            FormattingConfig.typedItemSpacing = overriding.typedItemSpacing |> Option.orElse this.typedItemSpacing
            typePrefixing = overriding.typePrefixing |> Option.orElse this.typePrefixing
            unionDefinitionIndentation = overriding.unionDefinitionIndentation |> Option.orElse this.unionDefinitionIndentation
            moduleDeclSpacing = overriding.moduleDeclSpacing |> Option.orElse this.moduleDeclSpacing
            classMemberSpacing = overriding.classMemberSpacing |> Option.orElse this.classMemberSpacing
            tupleFormatting = tryOverride this.tupleFormatting overriding.tupleFormatting (fun current overriding -> current.Override(overriding))
            patternMatchFormatting = tryOverride this.patternMatchFormatting overriding.patternMatchFormatting (fun current overriding -> current.Override(overriding))
        }

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

    member this.Override(overriding:RaiseWithTooManyArgsConfig) =
        {
            RaiseWithTooManyArgsConfig.raiseWithSingleArgument = overriding.raiseWithSingleArgument |> Option.orElse this.raiseWithSingleArgument
            nullArgWithSingleArgument = overriding.nullArgWithSingleArgument |> Option.orElse this.nullArgWithSingleArgument
            invalidOpWithSingleArgument = overriding.invalidOpWithSingleArgument |> Option.orElse this.invalidOpWithSingleArgument
            invalidArgWithTwoArguments = overriding.invalidArgWithTwoArguments |> Option.orElse this.invalidArgWithTwoArguments
            failwithfWithArgumentsMatchingFormatString = overriding.failwithfWithArgumentsMatchingFormatString |> Option.orElse this.failwithfWithArgumentsMatchingFormatString
        }

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

    member this.Override(overriding:SourceLengthConfig) =
        {
            SourceLengthConfig.maxLinesInLambdaFunction = overriding.maxLinesInLambdaFunction |> Option.orElse this.maxLinesInLambdaFunction
            maxLinesInMatchLambdaFunction = overriding.maxLinesInMatchLambdaFunction |> Option.orElse this.maxLinesInMatchLambdaFunction
            maxLinesInValue = overriding.maxLinesInValue |> Option.orElse this.maxLinesInValue
            maxLinesInFunction = overriding.maxLinesInFunction |> Option.orElse this.maxLinesInFunction
            maxLinesInMember = overriding.maxLinesInMember |> Option.orElse this.maxLinesInMember
            maxLinesInConstructor = overriding.maxLinesInConstructor |> Option.orElse this.maxLinesInConstructor
            maxLinesInProperty = overriding.maxLinesInProperty |> Option.orElse this.maxLinesInProperty
            maxLinesInModule = overriding.maxLinesInModule |> Option.orElse this.maxLinesInModule
            maxLinesInRecord = overriding.maxLinesInRecord |> Option.orElse this.maxLinesInRecord
            maxLinesInEnum = overriding.maxLinesInEnum |> Option.orElse this.maxLinesInEnum
            maxLinesInUnion = overriding.maxLinesInUnion |> Option.orElse this.maxLinesInUnion
            maxLinesInClass = overriding.maxLinesInClass |> Option.orElse this.maxLinesInClass
        }

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

    member this.Override(overriding:NamesConfig) =
        {
            NamesConfig.interfaceNames = overriding.interfaceNames |> Option.orElse this.interfaceNames
            exceptionNames = overriding.exceptionNames |> Option.orElse this.exceptionNames
            typeNames = overriding.typeNames |> Option.orElse this.typeNames
            recordFieldNames = overriding.recordFieldNames |> Option.orElse this.recordFieldNames
            enumCasesNames = overriding.enumCasesNames |> Option.orElse this.enumCasesNames
            unionCasesNames = overriding.unionCasesNames |> Option.orElse this.unionCasesNames
            moduleNames = overriding.moduleNames |> Option.orElse this.moduleNames
            literalNames = overriding.literalNames |> Option.orElse this.literalNames
            namespaceNames = overriding.namespaceNames |> Option.orElse this.namespaceNames
            memberNames = overriding.memberNames |> Option.orElse this.memberNames
            parameterNames = overriding.parameterNames |> Option.orElse this.parameterNames
            measureTypeNames = overriding.measureTypeNames |> Option.orElse this.measureTypeNames
            activePatternNames = overriding.activePatternNames |> Option.orElse this.activePatternNames
            publicValuesNames = overriding.publicValuesNames |> Option.orElse this.publicValuesNames
            nonPublicValuesNames = overriding.nonPublicValuesNames |> Option.orElse this.nonPublicValuesNames
        }

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

    member this.Override(overriding:NumberOfItemsConfig) =
        {
            NumberOfItemsConfig.maxNumberOfItemsInTuple = overriding.maxNumberOfItemsInTuple |> Option.orElse this.maxNumberOfItemsInTuple
            maxNumberOfFunctionParameters = overriding.maxNumberOfFunctionParameters |> Option.orElse this.maxNumberOfFunctionParameters
            maxNumberOfMembers = overriding.maxNumberOfMembers |> Option.orElse this.maxNumberOfMembers
            maxNumberOfBooleanOperatorsInCondition = overriding.maxNumberOfBooleanOperatorsInCondition |> Option.orElse this.maxNumberOfBooleanOperatorsInCondition
        }

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

    member this.Override(overriding:BindingConfig) =
        {
            BindingConfig.favourIgnoreOverLetWild = overriding.favourIgnoreOverLetWild |> Option.orElse this.favourIgnoreOverLetWild
            wildcardNamedWithAsPattern = overriding.wildcardNamedWithAsPattern |> Option.orElse this.wildcardNamedWithAsPattern
            uselessBinding = overriding.uselessBinding |> Option.orElse this.uselessBinding
            tupleOfWildcards = overriding.tupleOfWildcards |> Option.orElse this.tupleOfWildcards
        }

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

    member this.Override(overriding:ConventionsConfig) =
        {
            ConventionsConfig.recursiveAsyncFunction = overriding.recursiveAsyncFunction |> Option.orElse this.recursiveAsyncFunction
            redundantNewKeyword = overriding.redundantNewKeyword |> Option.orElse this.redundantNewKeyword
            nestedStatements = overriding.nestedStatements |> Option.orElse this.nestedStatements
            reimplementsFunction = overriding.reimplementsFunction |> Option.orElse this.reimplementsFunction
            canBeReplacedWithComposition = overriding.canBeReplacedWithComposition |> Option.orElse this.canBeReplacedWithComposition
            raiseWithTooManyArgs = tryOverride this.raiseWithTooManyArgs overriding.raiseWithTooManyArgs (fun current overriding -> current.Override(overriding))
            sourceLength = tryOverride this.sourceLength overriding.sourceLength (fun current overriding -> current.Override(overriding))
            naming = tryOverride this.naming overriding.naming (fun current overriding -> current.Override(overriding))
            numberOfItems = tryOverride this.numberOfItems overriding.numberOfItems (fun current overriding -> current.Override(overriding))
            binding = tryOverride this.binding overriding.binding (fun current overriding -> current.Override(overriding))
        }

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

    member this.Override(overriding:TypographyConfig) =
        {
            TypographyConfig.indentation = overriding.indentation |> Option.orElse this.indentation
            maxCharactersOnLine = overriding.maxCharactersOnLine |> Option.orElse this.maxCharactersOnLine
            trailingWhitespaceOnLine = overriding.trailingWhitespaceOnLine |> Option.orElse this.trailingWhitespaceOnLine
            maxLinesInFile = overriding.maxLinesInFile |> Option.orElse this.maxLinesInFile
            trailingNewLineInFile = overriding.trailingNewLineInFile |> Option.orElse this.trailingNewLineInFile
            noTabCharacters = overriding.noTabCharacters |> Option.orElse this.noTabCharacters
        }

let private getOrEmptyList hints = hints |> Option.defaultValue [||]

type HintConfig = {
    add : string [] option
    ignore : string [] option
} with
    member this.Override(overriding:HintConfig) =
        let newIgnore =
            overriding.ignore
            |> getOrEmptyList
            |> Array.append (this.ignore |> getOrEmptyList)
            |> Array.distinct
        let newAdd =
            overriding.add
            |> getOrEmptyList
            |> Array.append (this.add |> getOrEmptyList)
            |> Array.filter (fun hint -> not (Array.contains hint newIgnore))
            |> Array.distinct
        {
            HintConfig.add = Some newAdd
            ignore = Some newIgnore
        }

type Configuration =
    { ignoreFiles : string [] option
      formatting : FormattingConfig option
      conventions : ConventionsConfig option
      typography : TypographyConfig option
      hints : HintConfig option }
with
    member this.Override(overriding:Configuration) =
        {
            Configuration.ignoreFiles = tryOverride this.ignoreFiles overriding.ignoreFiles (fun current overriding -> current |> Array.append overriding |> Array.distinct)
            formatting = tryOverride this.formatting overriding.formatting (fun current overriding -> current.Override(overriding))
            conventions = tryOverride this.conventions overriding.conventions (fun current overriding -> current.Override(overriding))
            typography = tryOverride this.typography overriding.typography (fun current overriding -> current.Override(overriding))
            hints = tryOverride this.hints overriding.hints (fun current overriding -> current.Override(overriding))
        }

let mergeConfig (baseConfig : Configuration) (overridingConfig : Configuration) =
    baseConfig.Override(overridingConfig)

let parseConfig (configText : string) =
    try
        JsonConvert.DeserializeObject<Configuration>(configText, FSharpJsonConverter.serializerSettings)
    with
    | ex -> raise <| ConfigurationException(sprintf "Couldn't parse config, error=%s" ex.Message)

let serializeConfig (config : Configuration) =
    JsonConvert.SerializeObject(config, FSharpJsonConverter.serializerSettings)

type LineRules =
    { genericLineRules : RuleMetadata<LineRuleConfig> []
      noTabCharactersRule : RuleMetadata<NoTabCharactersRuleConfig> option
      indentationRule : RuleMetadata<IndentationRuleConfig> option }

type LoadedRules =
    { astNodeRules : RuleMetadata<AstNodeRuleConfig> []
      lineRules : LineRules }

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
    let allRules =
        [|
            config.formatting |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.conventions |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.typography |> Option.map (fun config -> config.Flatten()) |> Option.toArray |> Array.concat
            config.hints |> Option.map (fun config -> HintMatcher.rule { HintMatcher.Config.hintTrie = parseHints (getOrEmptyList config.add) }) |> Option.toArray
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
      lineRules =
          { genericLineRules = lineRules.ToArray()
            indentationRule = indentationRule
            noTabCharactersRule = noTabCharactersRule } }

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
