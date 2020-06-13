module FSharpLint.Rules.Helper.Naming

open System
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FSharpLint.Framework
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.SourceCodeServices

module QuickFixes =
    let removeAllUnderscores (ident:Ident) = lazy(
        let toText = ident.idText.Replace("_", "")
        Some { FromText = ident.idText; FromRange = ident.idRange; ToText = toText })

    let removeNonPrefixingUnderscores (ident:Ident) = lazy(
        let prefixingUnderscores =
            ident.idText |> Seq.takeWhile (fun x -> x = '_') |> String.Concat

        let toText = prefixingUnderscores + ident.idText.Replace("_", "")
        Some { FromText = ident.idText; FromRange = ident.idRange; ToText = toText })

    let addPrefix prefix (ident:Ident) = lazy(
        Some { FromText = ident.idText; FromRange = ident.idRange; ToText = prefix + ident.idText })

    let addSuffix suffix (ident:Ident) = lazy(
        Some { FromText = ident.idText; FromRange = ident.idRange; ToText = ident.idText + suffix })

    let private mapFirstChar map (str:string) =
        let prefix =
            str |> Seq.takeWhile (fun x -> x = '_') |> String.Concat
        let withoutPrefix = str.Substring prefix.Length
        if withoutPrefix.Length > 0 then
            let firstChar = map withoutPrefix.[0] |> string
            let rest = withoutPrefix.Substring 1
            prefix + firstChar + rest
        else ""

    let toPascalCase (ident:Ident) = lazy(
        let pascalCaseIdent = ident.idText |> mapFirstChar Char.ToUpper
        Some { FromText = ident.idText; FromRange = ident.idRange; ToText = pascalCaseIdent })

    let toCamelCase (ident:Ident) = lazy(
        let camelCaseIdent = ident.idText |> mapFirstChar Char.ToLower
        Some { FromText = ident.idText; FromRange = ident.idRange; ToText = camelCaseIdent })

[<Literal>]
let private NumberOfExpectedBackticks = 4

/// Is an identifier not surrounded by double backticks? e.g. not `let ``some identifier`` = 0`.
/// Unfortunately it's having to compare the length of the identifier in the source vs identifier length in AST,
/// the information as to whether the identifier was backticked doesn't appear to be in the AST.
let private isNotDoubleBackTickedIdent =
    let isDoubleBackTickedIdent (identifier:Ident) =
        let diffOfRangeAgainstIdent (r:range) = (r.EndColumn - r.StartColumn) - identifier.idText.Length

        let range = identifier.idRange
        not range.IsSynthetic && diffOfRangeAgainstIdent range = NumberOfExpectedBackticks

    isDoubleBackTickedIdent >> not

let private notOperator = isOperator >> not

let isPascalCase (identifier:string) =
    let withoutUnderscorePrefix = identifier.TrimStart '_'
    if withoutUnderscorePrefix.Length = 0 then true
    else Char.IsUpper withoutUnderscorePrefix.[0]

let isCamelCase (identifier:string) =
    let withoutUnderscorePrefix = identifier.TrimStart '_'
    if withoutUnderscorePrefix.Length = 0 then true
    else Char.IsLower withoutUnderscorePrefix.[0]

let private pascalCaseRule (identifier:string) =
    if not (isPascalCase identifier) then Some "RulesNamingConventionsPascalCaseError"
    else None

let private camelCaseRule (identifier:string) =
    if not (isCamelCase identifier) then Some "RulesNamingConventionsCamelCaseError"
    else None

let private underscoreRule allowPrefix (identifier:string) =
    if allowPrefix && identifier.TrimStart('_').Contains("_") then
        Some "RulesNamingConventionsUnderscoreError"
    else if not allowPrefix && identifier.Contains("_") then
        Some "RulesNamingConventionsUnderscoreError"
    else
        None

let private prefixRule (prefix:string) (identifier:string) =
    if not (identifier.StartsWith prefix) then Some "RulesNamingConventionsPrefixError"
    else None

let private suffixRule (suffix:string) (identifier:string) =
    if not (identifier.EndsWith suffix) then Some "RulesNamingConventionsSuffixError"
    else None

let private checkIdentifierPart (config:NamingConfig) (identifier:Ident) (idText:string) =
    let formatError errorName =
        String.Format(Resources.GetString errorName, idText)

    let formatError2 additional errorName =
        String.Format(Resources.GetString errorName, idText, additional)

    let tryAddFix fix message = (message, fix identifier)

    let casingError =
        match config.Naming with
        | Some NamingCase.PascalCase ->
            pascalCaseRule idText
            |> Option.map (formatError >> tryAddFix QuickFixes.toPascalCase)
        | Some NamingCase.CamelCase ->
            camelCaseRule idText
            |> Option.map (formatError >> tryAddFix QuickFixes.toCamelCase)
        | _ -> None

    let underscoresError =
        match config.Underscores with
        | Some NamingUnderscores.None ->
            underscoreRule false idText
            |> Option.map (formatError >> tryAddFix QuickFixes.removeAllUnderscores)
        | Some NamingUnderscores.AllowPrefix ->
            underscoreRule true idText
            |> Option.map (formatError >> tryAddFix QuickFixes.removeNonPrefixingUnderscores)
        | _ -> None

    let prefixError =
        config.Prefix
        |> Option.bind (fun prefix ->
            prefixRule prefix idText
            |> Option.map (formatError2 prefix >> tryAddFix (QuickFixes.addPrefix prefix)))

    let suffixError =
        config.Suffix
        |> Option.bind (fun suffix ->
            suffixRule suffix idText
            |> Option.map (formatError2 suffix >> tryAddFix (QuickFixes.addSuffix suffix)))

    [|
        casingError
        underscoresError
        prefixError
        suffixError
    |] |> Array.choose id

let private checkIdentifier (namingConfig:NamingConfig) (identifier:Ident) (idText:string) =
    if notOperator idText && isNotDoubleBackTickedIdent identifier then
        checkIdentifierPart namingConfig identifier idText
        |> Array.map (fun (message, suggestedFix) ->
            { Range = identifier.idRange
              Message = message
              SuggestedFix = Some suggestedFix
              TypeChecks = [] })
    else
        Array.empty

let toAstNodeRule (namingRule:RuleMetadata<NamingRuleConfig>) =
    let astNodeRunner (args:AstNodeRuleParams) =
        namingRule.RuleConfig.GetIdentifiersToCheck args
        |> Array.collect (fun (identifier, idText, typeCheck) ->
            let suggestions = checkIdentifier namingRule.RuleConfig.Config identifier idText
            suggestions |> Array.map (fun suggestion -> { suggestion with TypeChecks = Option.toList typeCheck }))

    {
        RuleMetadata.Name = namingRule.Name
        Identifier = namingRule.Identifier
        RuleConfig = { AstNodeRuleConfig.Runner = astNodeRunner; Cleanup = id }
    }

let isActivePattern (identifier:Ident) =
    FSharp.Compiler.PrettyNaming.IsActivePatternName identifier.idText

let activePatternIdentifiers (identifier:Ident) =
    identifier.idText.Split('|')
    |> Seq.toArray
    |> Array.filter (fun x -> not <| String.IsNullOrEmpty(x) && x.Trim() <> "_")

let isPublic (syntaxArray:AbstractSyntaxArray.Node []) i =
    let isSynAccessPublic = function
        | Some(SynAccess.Public) | None -> true
        | _ -> false

    let rec isPublic publicSoFar isPrivateWhenReachedBinding i =
        if i = 0 then publicSoFar
        else if publicSoFar then
            let node = syntaxArray.[i]
            match node.Actual with
            | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.Record(access, _, _))
            | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.Union(access, _, _))
            | UnionCase(SynUnionCase.UnionCase(_, _, _, _, access, _))
            | Field(SynField.Field(_, _, _, _, _, _, access, _))
            | ComponentInfo(SynComponentInfo.ComponentInfo(_, _, _, _, _, _, access, _))
            | ModuleOrNamespace (SynModuleOrNamespace.SynModuleOrNamespace(_, _, _, _, _, _, access, _))
            | ExceptionRepresentation(SynExceptionDefnRepr.SynExceptionDefnRepr(_, _, _, _, access, _))
            | Pattern(SynPat.Named(_, _, _, access, _))
            | Pattern(SynPat.LongIdent(_, _, _, _, access, _)) ->
                isPublic (isSynAccessPublic access) isPrivateWhenReachedBinding node.ParentIndex
            | TypeSimpleRepresentation(_)
            | Pattern(_) -> true
            | MemberDefinition(_) ->
                if isPrivateWhenReachedBinding then false
                else isPublic publicSoFar isPrivateWhenReachedBinding node.ParentIndex
            | Binding(SynBinding.Binding(access, _, _, _, _, _, _, _, _, _, _, _)) ->
                if isPrivateWhenReachedBinding then false
                else isPublic (isSynAccessPublic access) true node.ParentIndex
            | EnumCase(_)
            | TypeRepresentation(_)
            | Type(_)
            | Match(_)
            | ConstructorArguments(_)
            | TypeParameter(_)
            | InterfaceImplementation(_)
            | ModuleDeclaration(_)
            | Identifier(_)
            | SimplePattern(_)
            | File(_)
            | LambdaArg(_)
            | SimplePatterns(_) -> isPublic publicSoFar isPrivateWhenReachedBinding node.ParentIndex
            | TypeDefinition(_)
            | Else(_)
            | LambdaBody(_)
            | Expression(_) -> isPublic publicSoFar true node.ParentIndex
        else false

    isPublic true false i

/// Is an attribute with a given name?
/// e.g. check for Literal attribute.
let isAttribute name (attributes:SynAttributes) =
    let fullName = name + "Attribute"

    let attributeHasExpectedName (attribute:SynAttribute) =
        match List.tryLast attribute.TypeName.Lid with
        | Some(ident) -> ident.idText = fullName || ident.idText = name
        | None -> false

    attributes
    |> extractAttributes
    |> List.exists attributeHasExpectedName

let isLiteral = isAttribute "Literal"

let isMeasureType = isAttribute "Measure"

let isNotUnionCase (checkFile:FSharpCheckFileResults) (ident:Ident) = async {
    let! symbol = checkFile.GetSymbolUseAtLocation(
                    ident.idRange.StartLine, ident.idRange.EndColumn, "", [ident.idText])

    return
        match symbol with
        | Some(symbol) when (symbol.Symbol :? FSharpUnionCase) -> false
        | Some(_) | None -> true }

let isInterface typeDef =
    let hasConstructor = function
        | SynMemberDefn.ImplicitCtor(_) -> true
        | _ -> false

    let canBeInInterface = function
        | SynMemberDefn.Open(_)
        | SynMemberDefn.AbstractSlot(_)
        | SynMemberDefn.Inherit(_) -> true
        | _ -> false

    match typeDef with
    | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconInterface, members, _)
    | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconUnspecified, members, _) ->
        members |> List.exists hasConstructor |> not &&
        members |> List.forall canBeInInterface
    | _ -> false

let checkIfPublic isCurrentlyPublic = function
    | Some(SynAccess.Public) | None -> isCurrentlyPublic
    | Some(SynAccess.Internal | SynAccess.Private) -> false

let isModule (moduleKind:SynModuleOrNamespaceKind) =
    match moduleKind with
    | AnonModule
    | NamedModule -> true
    | DeclaredNamespace
    | GlobalNamespace -> false

/// Is module name implicitly created from file name?
let isImplicitModule (SynModuleOrNamespace.SynModuleOrNamespace(longIdent, _, moduleKind, _, _, _, _, range)) =
    let zeroLengthRange (r:range) =
        (r.EndColumn - r.StartColumn) = 0 && r.StartLine = r.EndLine

    // Check the identifiers in the module name have no length.
    // Not ideal but there's no attribute in the AST indicating the module is implicit from the file name.
    // TODO: does SynModuleOrNamespaceKind.AnonModule replace this check?
    isModule moduleKind && longIdent |> List.forall (fun x -> zeroLengthRange x.idRange)

let rec getPatternIdents isPublic getIdents argsAreParameters pattern =
    match pattern with
    | SynPat.LongIdent(_, _, _, args, access, _) ->
        let isPublic = checkIfPublic isPublic access

        let hasNoArgs =
            match args with
            | SynArgPats.NamePatPairs(pats, _) -> pats.IsEmpty
            | SynArgPats.Pats(pats) -> pats.IsEmpty

        let argSuggestions =
            match args with
            | SynArgPats.NamePatPairs(pats, _) ->
                pats
                |> List.toArray
                |> Array.collect (snd >> getPatternIdents false getIdents false)
            | SynArgPats.Pats(pats) ->
                pats
                |> List.toArray
                |> Array.collect (getPatternIdents false getIdents false)

        // Only check if expecting args as parameters e.g. function - otherwise is a DU pattern.
        if hasNoArgs || argsAreParameters then
            getIdents isPublic pattern
            |> Array.append argSuggestions
        else
            argSuggestions
    | SynPat.Named(p, _, _, access, _) ->
        let isPublic = checkIfPublic isPublic access
        getIdents isPublic pattern
        |> Array.append (getPatternIdents isPublic getIdents false p)
    | SynPat.Or(p1, p2, _) ->
        [|p1; p2|]
        |> Array.collect (getPatternIdents isPublic getIdents false)
    | SynPat.Paren(p, _) ->
        getPatternIdents isPublic getIdents false p
    | SynPat.Ands(pats, _)
    | SynPat.Tuple(_, pats, _)
    | SynPat.ArrayOrList(_, pats, _) ->
        pats
        |> List.toArray
        |> Array.collect (getPatternIdents isPublic getIdents false)
    | SynPat.Record(_)
    | SynPat.IsInst(_)
    | SynPat.QuoteExpr(_)
    | SynPat.Null(_)
    | SynPat.Typed(_)
    | SynPat.Attrib(_)
    | SynPat.Const(_)
    | SynPat.Wild(_)
    | SynPat.OptionalVal(_)
    | SynPat.DeprecatedCharRange(_) | SynPat.InstanceMember(_) | SynPat.FromParseError(_) -> Array.empty

let rec identFromSimplePat = function
    | SynSimplePat.Id(ident, _, _, _, _, _) -> Some(ident)
    | SynSimplePat.Typed(p, _, _) -> identFromSimplePat p
    | SynSimplePat.Attrib(_) -> None
