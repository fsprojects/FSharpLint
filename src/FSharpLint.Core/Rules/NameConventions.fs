namespace FSharpLint.Rules

module private Option =
    let filter f = function None -> None | Some x -> if f x then Some x else None

/// Checks whether any code in an F# program violates best practices for naming identifiers.
module NameConventions =

    open System
    open System.Linq
    open System.Text.RegularExpressions
    open FSharp.Compiler.Ast
    open FSharp.Compiler.Range
    open FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.AstInfo
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "NameConventions"

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

    let private isActivePattern (identifier:Ident) =
        FSharp.Compiler.PrettyNaming.IsActivePatternName identifier.idText

    let private activePatternIdentifiers (identifier:Ident) =
        identifier.idText.Split('|')
        |> Seq.filter (fun x -> not <| String.IsNullOrEmpty(x) && x.Trim() <> "_")

    module QuickFixes =
        let removeAllUnderscores (ident: Ident) = lazy(
            let toText = ident.idText.Replace("_", "")
            Some { FromText = ident.idText; FromRange = ident.idRange; ToText = toText })

        let removeNonPrefixingUnderscores (ident: Ident) = lazy(
            let prefixingUnderscores =
                ident.idText |> Seq.takeWhile (fun x -> x = '_') |> String.Concat

            let toText = prefixingUnderscores + ident.idText.Replace("_", "")
            Some { FromText = ident.idText; FromRange = ident.idRange; ToText = toText })

        let addPrefix prefix (ident: Ident) = lazy(
            Some { FromText = ident.idText; FromRange = ident.idRange; ToText = prefix + ident.idText })

        let addSuffix suffix (ident: Ident) = lazy(
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

        let toPascalCase (ident: Ident) = lazy(
            let pascalCaseIdent = ident.idText |> mapFirstChar Char.ToUpper
            Some { FromText = ident.idText; FromRange = ident.idRange; ToText = pascalCaseIdent })

        let toCamelCase (ident: Ident) = lazy(
            let camelCaseIdent = ident.idText |> mapFirstChar Char.ToLower
            Some { FromText = ident.idText; FromRange = ident.idRange; ToText = camelCaseIdent })

    type private NamingRule =
        { Name: string
          Casing: Naming option
          Underscores: NamingUnderscores option
          Prefix: string option
          Suffix: string option }

        member private this.CheckIdentifierPart (identifier: Ident) (str: string) =
            let formatError errorName =
                String.Format(Resources.GetString errorName, str)

            let formatError2 additional errorName =
                String.Format(Resources.GetString errorName, str, additional)

            let tryAddFix fix message = (message, fix identifier)

            [ match this.Casing with
              | Some(Naming.PascalCase) ->
                  yield pascalCaseRule str
                  |> Option.map (formatError >> tryAddFix QuickFixes.toPascalCase)
              | Some(Naming.CamelCase) ->
                  yield camelCaseRule str
                  |> Option.map (formatError >> tryAddFix QuickFixes.toCamelCase)
              | _ -> ()

              match this.Underscores with
              | Some(NamingUnderscores.None) ->
                  yield underscoreRule false str
                  |> Option.map (formatError >> tryAddFix QuickFixes.removeAllUnderscores)
              | Some(NamingUnderscores.AllowPrefix) ->
                  yield underscoreRule true str
                  |> Option.map (formatError >> tryAddFix QuickFixes.removeNonPrefixingUnderscores)
              | _ -> ()

              match this.Prefix with
              | Some(prefix) ->
                  yield prefixRule prefix str
                  |> Option.map (formatError2 prefix >> tryAddFix (QuickFixes.addPrefix prefix))
              | None -> ()

              match this.Suffix with
              | Some(suffix) ->
                  yield suffixRule suffix str
                  |> Option.map (formatError2 suffix >> tryAddFix (QuickFixes.addSuffix suffix))
              | None -> () ]

        member this.Check (identifier: Ident) =
            if this.Name = "ActivePatternNames" then
                activePatternIdentifiers identifier
                |> Seq.collect (this.CheckIdentifierPart identifier)
                |> Seq.choose id
                |> Seq.toList
            else
                this.CheckIdentifierPart identifier identifier.idText |> List.choose id

        static member Build ruleName settings =
            { Name = ruleName
              Casing =
                match Map.tryFind "Naming" settings with
                | Some(Naming(Naming.PascalCase)) -> Some Naming.PascalCase
                | Some(Naming(Naming.CamelCase)) -> Some Naming.CamelCase
                | _ -> None
              Underscores =
                match Map.tryFind "Underscores" settings with
                | Some(Underscores(NamingUnderscores.AllowPrefix)) -> Some NamingUnderscores.AllowPrefix
                | Some(Underscores(NamingUnderscores.None)) -> Some NamingUnderscores.None
                | Some(Underscores(NamingUnderscores.AllowAny)) | _ -> None
              Prefix =
                match Map.tryFind "Prefix" settings with
                | Some(Prefix(prefix)) -> prefix
                | _ -> None
              Suffix =
                match Map.tryFind "Suffix" settings with
                | Some(Suffix(suffix)) -> suffix
                | _ -> None }

    type private Rules =
        { InterfaceNames: NamingRule option
          ExceptionNames: NamingRule option
          TypeNames: NamingRule option
          RecordFieldNames: NamingRule option
          EnumCasesNames: NamingRule option
          UnionCasesNames: NamingRule option
          ModuleNames: NamingRule option
          LiteralNames: NamingRule option
          NamespaceNames: NamingRule option
          MemberNames: NamingRule option
          ParameterNames: NamingRule option
          MeasureTypeNames: NamingRule option
          ActivePatternNames: NamingRule option
          PublicValuesNames: NamingRule option
          NonPublicValuesNames: NamingRule option }

        static member Build settings =
            let getSettings ruleName =
                Configuration.isRuleEnabled settings AnalyserName ruleName
                |> Option.map (snd >> NamingRule.Build ruleName)

            { InterfaceNames = getSettings "InterfaceNames"
              ExceptionNames = getSettings "ExceptionNames"
              TypeNames = getSettings "TypeNames"
              RecordFieldNames = getSettings "RecordFieldNames"
              EnumCasesNames = getSettings "EnumCasesNames"
              UnionCasesNames = getSettings "UnionCasesNames"
              ModuleNames = getSettings "ModuleNames"
              LiteralNames = getSettings "LiteralNames"
              NamespaceNames = getSettings "NamespaceNames"
              MemberNames = getSettings "MemberNames"
              ParameterNames = getSettings "ParameterNames"
              MeasureTypeNames = getSettings "MeasureTypeNames"
              ActivePatternNames = getSettings "ActivePatternNames"
              PublicValuesNames = getSettings "PublicValuesNames"
              NonPublicValuesNames = getSettings "NonPublicValuesNames" }

    let private isPublic (syntaxArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) i =
        let isSynAccessPublic = function
            | Some(SynAccess.Public) | None -> true
            | _ -> false

        let rec isPublic publicSoFar isPrivateWhenReachedBinding i =
            if i = 0 then publicSoFar
            else if publicSoFar then
                match syntaxArray.[i].Actual with
                | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.Record(access, _, _))
                | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.Union(access, _, _))
                | UnionCase(SynUnionCase.UnionCase(_, _, _, _, access, _))
                | Field(SynField.Field(_, _, _, _, _, _, access, _))
                | ComponentInfo(SynComponentInfo.ComponentInfo(_, _, _, _, _, _, access, _))
                | ModuleOrNamespace (SynModuleOrNamespace.SynModuleOrNamespace(_, _, _, _, _, _, access, _))
                | ExceptionRepresentation(SynExceptionDefnRepr.SynExceptionDefnRepr(_, _, _, _, access, _))
                | Pattern(SynPat.Named(_, _, _, access, _))
                | Pattern(SynPat.LongIdent(_, _, _, _, access, _)) ->
                    isPublic (isSynAccessPublic access) isPrivateWhenReachedBinding skipArray.[i].ParentIndex
                | TypeSimpleRepresentation(_)
                | Pattern(_) -> true
                | MemberDefinition(_) ->
                    if isPrivateWhenReachedBinding then false
                    else isPublic publicSoFar isPrivateWhenReachedBinding skipArray.[i].ParentIndex
                | Binding(SynBinding.Binding(access, _, _, _, _, _, _, _, _, _, _, _)) ->
                    if isPrivateWhenReachedBinding then false
                    else isPublic (isSynAccessPublic access) true skipArray.[i].ParentIndex
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
                | SimplePatterns(_) -> isPublic publicSoFar isPrivateWhenReachedBinding skipArray.[i].ParentIndex
                | TypeDefinition(_)
                | Expression(_) -> isPublic publicSoFar true skipArray.[i].ParentIndex
            else false

        isPublic true false i

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

    /// Is an attribute with a given name?
    /// e.g. check for Literal attribute.
    let private isAttribute name (attributes:SynAttributes) =
        let fullName = name + "Attribute"

        let attributeHasExpectedName (attribute:SynAttribute) =
            match List.tryLast attribute.TypeName.Lid with
            | Some(ident) -> ident.idText = fullName || ident.idText = name
            | None -> false

        attributes |> List.exists attributeHasExpectedName

    let private isLiteral = isAttribute "Literal"

    let private isMeasureType = isAttribute "Measure"

    let private isNotUnionCase (checkFile:FSharpCheckFileResults) (ident:Ident) = async {
        let! symbol = checkFile.GetSymbolUseAtLocation(
                        ident.idRange.StartLine, ident.idRange.EndColumn, "", [ident.idText])

        return
            match symbol with
            | Some(symbol) when (symbol.Symbol :? FSharpUnionCase) -> false
            | Some(_) | None -> true }

    let private isInterface typeDef =
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

    let private checkIfPublic isCurrentlyPublic = function
        | Some(SynAccess.Public) | None -> isCurrentlyPublic
        | Some(SynAccess.Internal | SynAccess.Private) -> false

    let private checkValueOrFunction checkRule rules typeChecker isPublic pattern =
        let checkNotUnionCase ident =
            typeChecker |> Option.map (fun checker -> isNotUnionCase checker ident)

        match pattern with
        | SynPat.LongIdent(longIdent, _, _, _, _, _) ->
            // If a pattern identifier is made up of more than one part then it's not binding a new value.
            let singleIdentifier = List.length longIdent.Lid = 1

            match List.tryLast longIdent.Lid with
            | Some(ident) when isActivePattern ident ->
                checkRule rules.ActivePatternNames ident
            | Some(ident) when singleIdentifier ->
                let checkNotUnionCase = checkNotUnionCase ident
                if isPublic then
                    checkRule rules.PublicValuesNames ident
                else
                    checkRule rules.NonPublicValuesNames ident
                |> List.map (fun (x:Analyser.LintSuggestion) -> x.WithTypeCheck checkNotUnionCase)
            | None | Some(_) -> []
        | SynPat.Named(_, ident, _, _, _)
        | SynPat.OptionalVal(ident, _) ->
            if isActivePattern ident then
                checkRule rules.ActivePatternNames ident
            else
                let checkNotUnionCase = checkNotUnionCase ident
                checkRule rules.ParameterNames ident
                |> List.map (fun (x:Analyser.LintSuggestion) -> x.WithTypeCheck checkNotUnionCase)
        | _ -> []

    let private checkMember checkRule rules _ = function
        | SynPat.LongIdent(longIdent, _, _, _, _, _) ->
            match List.tryLast longIdent.Lid with
            | Some(ident) -> checkRule rules.MemberNames ident
            | None -> []
        | SynPat.Named(_, ident, _, _, _)
        | SynPat.OptionalVal(ident, _) ->
            checkRule rules.ParameterNames ident
        | _ -> []

    let rec private checkPattern isPublic checker argsAreParameters pattern =
        match pattern with
        | SynPat.LongIdent(_, _, _, args, access, _) ->
            let isPublic = checkIfPublic isPublic access

            let hasNoArgs =
                match args with
                | SynConstructorArgs.NamePatPairs(pats, _) -> pats.IsEmpty
                | SynConstructorArgs.Pats(pats) -> pats.IsEmpty

            let argSuggestions =
                match args with
                | SynConstructorArgs.NamePatPairs(pats, _) ->
                    pats |> List.collect (snd >> checkPattern false checker false)
                | SynConstructorArgs.Pats(pats) ->
                    pats |> List.collect (checkPattern false checker false)

            // Only check if expecting args as parameters e.g. function - otherwise is a DU pattern.
            if hasNoArgs || argsAreParameters then
                checker isPublic pattern @ argSuggestions
            else
                argSuggestions
        | SynPat.Named(p, _, _, access, _) ->
            let isPublic = checkIfPublic isPublic access
            checker isPublic pattern @ checkPattern isPublic checker false p
        | SynPat.Or(p1, p2, _) ->
            [p1; p2] |> List.collect (checkPattern isPublic checker false)
        | SynPat.Paren(p, _) ->
            checkPattern isPublic checker false p
        | SynPat.Ands(pats, _)
        | SynPat.Tuple(_, pats, _)
        | SynPat.ArrayOrList(_, pats, _) ->
            pats |> List.collect (checkPattern isPublic checker false)
        | SynPat.Record(_)
        | SynPat.IsInst(_)
        | SynPat.QuoteExpr(_)
        | SynPat.Null(_)
        | SynPat.Typed(_)
        | SynPat.Attrib(_)
        | SynPat.Const(_)
        | SynPat.Wild(_)
        | SynPat.OptionalVal(_)
        | SynPat.DeprecatedCharRange(_) | SynPat.InstanceMember(_) | SynPat.FromParseError(_) -> []

    let rec private identFromSimplePat = function
        | SynSimplePat.Id(ident, _, _, _, _, _) -> Some(ident)
        | SynSimplePat.Typed(p, _, _) -> identFromSimplePat p
        | SynSimplePat.Attrib(_) -> None

    let isModule (moduleKind: SynModuleOrNamespaceKind) =
        match moduleKind with
            | AnonModule
            | NamedModule -> true
            | DeclaredNamespace
            | GlobalNamespace -> false

    /// Is module name implicitly created from file name?
    let private isImplicitModule (SynModuleOrNamespace.SynModuleOrNamespace(longIdent, _, moduleKind, _, _, _, _, range)) =
        let zeroLengthRange (r:range) =
            (r.EndColumn - r.StartColumn) = 0 && r.StartLine = r.EndLine

        // Check the identifiers in the module name have no length.
        // Not ideal but there's no attribute in the AST indicating the module is implicit from the file name.
        // TODO: does SynModuleOrNamespaceKind.AnonModule replace this check?
        isModule moduleKind && longIdent |> List.forall (fun x -> zeroLengthRange x.idRange)

    let analyser (args: AnalyserArgs) : unit =
        let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

        let rules = Rules.Build args.Info.Config

        let isNotSuppressed i ruleName =
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
            |> not

        let checkNamingRule i (rule: NamingRule option) (identifier:Ident) =
            match rule with
            | Some(rule) when notOperator identifier.idText && isNotDoubleBackTickedIdent identifier ->
                rule.Check identifier
                |> List.choose (fun (message, suggestedFix) ->
                    if isNotSuppressed i rule.Name then
                        Some
                            { Range = identifier.idRange
                              Message = message
                              SuggestedFix = Some suggestedFix
                              TypeChecks = [] }
                    else None)
            | _ -> []

        let suggest suggestion = args.Info.Suggest suggestion

        for i = 0 to syntaxArray.Length - 1 do
            let checkRule = checkNamingRule i

            match syntaxArray.[i].Actual with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, _, moduleKind, _, _, _, _, _) as synModule) ->
                if not <| isImplicitModule synModule then
                    let checkIdent =
                        if isModule moduleKind then checkRule rules.ModuleNames
                        else checkRule rules.NamespaceNames

                    identifier |> List.collect checkIdent |> List.iter args.Info.Suggest
            | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
                checkRule rules.UnionCasesNames identifier |> List.iter args.Info.Suggest
            | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
                identifier
                |> Option.map (checkRule rules.RecordFieldNames)
                |> Option.iter (List.iter args.Info.Suggest)
            | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
                checkRule rules.EnumCasesNames identifier |> List.iter args.Info.Suggest
            | AstNode.ExceptionRepresentation(SynExceptionDefnRepr.SynExceptionDefnRepr(_, unionCase, _, _, _, _)) ->
                match unionCase with
                | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
                    checkRule rules.ExceptionNames identifier |> List.iter args.Info.Suggest
            | AstNode.Expression(SynExpr.For(_, identifier, _, _, _, _, _)) ->
                checkRule rules.NonPublicValuesNames  identifier |> List.iter args.Info.Suggest
            | AstNode.Expression(SynExpr.ForEach(_, _, true, pattern, _, _, _)) ->
                checkPattern false (checkValueOrFunction checkRule rules args.CheckFile) false pattern
                |> List.iter suggest
            | AstNode.MemberDefinition(memberDef) ->
                match memberDef with
                | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, identifier, _, _, _, _, _, _, _, _, _), _, _) ->
                    checkRule rules.MemberNames  identifier |> List.iter args.Info.Suggest
                | SynMemberDefn.ImplicitCtor(_, _, ctorArgs, _, _) ->
                    for arg in ctorArgs do
                        identFromSimplePat arg
                        |> Option.map (checkRule rules.ParameterNames)
                        |> Option.iter (List.iter args.Info.Suggest)
                | _ -> ()
            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(componentInfo, typeDef, _, _)) ->
                let isNotTypeExtension =
                    match typeDef with
                    | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconAugmentation, _, _) -> false
                    | _ -> true

                if isNotTypeExtension then
                    match componentInfo with
                    | SynComponentInfo.ComponentInfo(attrs, _, _, identifier, _, _, _, _) ->
                        match List.tryLast identifier with
                        | Some(typeIdentifier) ->
                            if isMeasureType attrs then
                                checkRule rules.MeasureTypeNames typeIdentifier
                                |> List.iter args.Info.Suggest
                            else if isInterface typeDef then
                                checkRule rules.InterfaceNames typeIdentifier
                                |> List.iter args.Info.Suggest
                            else
                                identifier
                                |> List.collect (checkRule rules.TypeNames)
                                |> List.iter args.Info.Suggest
                        | _ -> ()
            | AstNode.Binding(SynBinding.Binding(access, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
                if isLiteral attributes then
                    let rec checkLiteral = function
                    | SynPat.Named(_, identifier, _, _, _) ->
                        checkRule rules.LiteralNames identifier
                    | SynPat.Paren(p, _) -> checkLiteral p
                    | _ -> []

                    checkLiteral pattern |> List.iter args.Info.Suggest
                else
                    match identifierTypeFromValData valData with
                    | Value | Function ->
                        let isPublic = isPublic syntaxArray skipArray i
                        checkPattern isPublic (checkValueOrFunction checkRule rules args.CheckFile) true pattern
                        |> List.iter suggest
                    | Member | Property ->
                        checkPattern false (checkMember checkRule rules) true pattern
                        |> List.iter suggest
                    | _ -> ()
            | AstNode.Match(SynMatchClause.Clause(pattern, _, _, _, _)) ->
                match pattern with
                | SynPat.Named(_, identifier, isThis, _, _) when not isThis ->
                    checkRule rules.NonPublicValuesNames identifier |> List.iter args.Info.Suggest
                | _ -> ()
            | _ -> ()
