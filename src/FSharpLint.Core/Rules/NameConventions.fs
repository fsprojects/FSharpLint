// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.Rules

module private Option =
    let filter f = function None -> None | Some x -> if f x then Some x else None

/// Checks whether any code in an F# program violates best practices for naming identifiers.
module NameConventions =

    open System
    open System.Linq
    open System.Text.RegularExpressions
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.AstInfo
    open FSharpLint.Framework.Configuration

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
    let AnalyserName = "NameConventions"

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
        if identifier.Contains "_" then
            if not allowPrefix then
                Some "RulesNamingConventionsUnderscoreError"
            else if identifier.TrimStart('_').Contains("_") then
                Some "RulesNamingConventionsUnderscoreError"
            else
                None
        else None

    let private prefixRule prefix (identifier:string) =
        if not (identifier.StartsWith prefix) then Some "RulesNamingConventionsPrefixError"
        else None

    let private suffixRule suffix (identifier:string) =
        if not (identifier.EndsWith suffix) then Some "RulesNamingConventionsSuffixError"
        else None

    let private notOperator = isOperator >> not

    module private CheckIdentifiers =
        type Rules =
            | InterfaceNames
            | ExceptionNames
            | TypeNames
            | RecordFieldNames
            | EnumCasesNames
            | UnionCasesNames
            | ModuleNames
            | LiteralNames
            | NamespaceNames
            | MemberNames
            | ParameterNames
            | MeasureTypeNames
            | ActivePatternNames
            | PublicValuesNames
            | NonPublicValuesNames

        let toString = function
            | InterfaceNames -> "InterfaceNames"
            | ExceptionNames -> "ExceptionNames"
            | TypeNames -> "TypeNames"
            | RecordFieldNames -> "RecordFieldNames"
            | EnumCasesNames -> "EnumCasesNames"
            | UnionCasesNames -> "UnionCasesNames"
            | ModuleNames -> "ModuleNames"
            | LiteralNames -> "LiteralNames"
            | NamespaceNames -> "NamespaceNames"
            | MemberNames -> "MemberNames"
            | ParameterNames -> "ParameterNames"
            | MeasureTypeNames -> "MeasureTypeNames"
            | ActivePatternNames -> "ActivePatternNames"
            | PublicValuesNames -> "PublicValuesNames"
            | NonPublicValuesNames -> "NonPublicValuesNames"

    let private isActivePattern (identifier:Ident) =
        Microsoft.FSharp.Compiler.PrettyNaming.IsActivePatternName identifier.idText

    let private activePatternIdentifiers (identifier:Ident) =
            identifier.idText.Split('|')
            |> Seq.filter (fun x -> not <| String.IsNullOrEmpty(x) && x.Trim() <> "_")

    /// Is an attribute from FSharp.Core with a given name?
    /// e.g. check for Literal attribute.
    let private isCoreAttribute name (attributes:SynAttributes) (checkFile:FSharpCheckFileResults option) =
        let fullName = name + "Attribute"

        match checkFile with
        | Some(checkFile) ->
            let isAttributeFromCore (attribute:SynAttribute) =
                let range = attribute.TypeName.Range
                let names = attribute.TypeName.Lid |> List.map (fun x -> x.idText)

                let symbol =
                    checkFile.GetSymbolUseAtLocation(range.EndLine + 1, range.EndColumn, "", names)
                    |> Async.RunSynchronously

                match symbol with
                | Some(symbol) ->
                    match symbol.Symbol with
                    | :? FSharpEntity as entity when
                            entity.IsFSharpAbbreviation &&
                            entity.AbbreviatedType.TypeDefinition.DisplayName = fullName ->
                        match entity.AbbreviatedType.TypeDefinition.Namespace with
                        | Some(name) when name.EndsWith("FSharp.Core") -> true
                        | _ -> false
                    | :? FSharpEntity as entity when
                            entity.IsClass && entity.DisplayName = fullName ->
                        match entity.Namespace with
                        | Some(name) when name.EndsWith("FSharp.Core") -> true
                        | _ -> false
                    | _ -> false
                | _ -> false

            attributes |> List.exists isAttributeFromCore
        | None ->
            let attributeHasExpectedName (attribute:SynAttribute) =
                match attribute.TypeName.Lid |> List.rev with
                | ident::_ -> ident.idText = fullName || ident.idText = name
                | [] -> false

            attributes |> List.exists attributeHasExpectedName

    let private isLiteral = isCoreAttribute "Literal"

    let private isMeasureType = isCoreAttribute "Measure"

    let private isUnionCase (checkFile:FSharpCheckFileResults) (ident:Ident) =
        let symbol = checkFile.GetSymbolUseAtLocation(
                        ident.idRange.StartLine,
                        ident.idRange.EndColumn,
                        "",
                        [ident.idText])
                        |> Async.RunSynchronously

        match symbol with
        | Some(symbol) when (symbol.Symbol :? FSharpUnionCase) -> true
        | Some(_) | None -> false

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

    let private checkLongIdent checkRule valData isPublic = function
        | SynPat.LongIdent(longIdentifier, _, _, args, access, _) ->
            let isPublic = function
                | Some(access) -> access = SynAccess.Public && isPublic ()
                | None -> isPublic ()

            match args with
            | SynConstructorArgs.NamePatPairs(_) -> ()
            | SynConstructorArgs.Pats(_) -> ()

            match longIdentifier.Lid |> List.rev with
            | lastIdent::_ ->
                match identifierTypeFromValData valData with
                | Value | Function when isActivePattern lastIdent ->
                    CheckIdentifiers.ActivePatternNames |> checkRule lastIdent
                | Value | Function when isPublic access ->
                    CheckIdentifiers.PublicValuesNames |> checkRule lastIdent
                | Value | Function ->
                    CheckIdentifiers.NonPublicValuesNames |> checkRule lastIdent
                | Member | Property ->
                    CheckIdentifiers.MemberNames |> checkRule lastIdent
                | _ -> ()
            | _ -> ()
        | _ -> ()

    let private checkIfPublic isCurrentlyPublic = function
        | Some(access) -> isCurrentlyPublic && access = SynAccess.Public
        | None -> isCurrentlyPublic

    let private checkValueOrFunction checkRule typeChecker isPublic pattern =
        let isUnionCase ident =
            match typeChecker with
            | Some(typeChecker) -> isUnionCase typeChecker ident
            | None -> false

        match pattern with
        | SynPat.LongIdent(longIdent, _, _, _, _, _) ->
            if not longIdent.Lid.IsEmpty then
                let ident = longIdent.Lid.Last()

                if isActivePattern ident then
                    checkRule CheckIdentifiers.ActivePatternNames ident
                else if not <| isUnionCase ident then
                    if isPublic then
                        checkRule CheckIdentifiers.PublicValuesNames ident
                    else
                        checkRule CheckIdentifiers.NonPublicValuesNames ident
        | SynPat.Named(_, ident, _, _, _)
        | SynPat.OptionalVal(ident, _) ->
            if isActivePattern ident then
                checkRule CheckIdentifiers.ActivePatternNames ident
            else if not <| isUnionCase ident then
                checkRule CheckIdentifiers.ParameterNames ident
        | _ -> ()

    let private checkMember checkRule _ = function
        | SynPat.LongIdent(longIdent, _, _, _, _, _) ->
            if not longIdent.Lid.IsEmpty then
                let ident = longIdent.Lid.Last()
                checkRule CheckIdentifiers.MemberNames ident
        | SynPat.Named(_, ident, _, _, _)
        | SynPat.OptionalVal(ident, _) ->
            checkRule CheckIdentifiers.ParameterNames ident
        | _ -> ()

    let rec private checkPattern isPublic checker argsAreParameters pattern =
        match pattern with
        | SynPat.OptionalVal(_) -> ()
        | SynPat.LongIdent(_, _, _, args, access, _) ->
            let isPublic = checkIfPublic isPublic access

            let hasNoArgs =
                match args with
                | SynConstructorArgs.NamePatPairs(pats, _) -> pats.IsEmpty
                | SynConstructorArgs.Pats(pats) -> pats.IsEmpty

            // Only check if expecting args as parameters e.g. function - otherwise is a DU pattern.
            if hasNoArgs || argsAreParameters then
                checker isPublic pattern

            match args with
            | SynConstructorArgs.NamePatPairs(pats, _) ->
                for (_, pat) in pats do
                    checkPattern false checker false pat
            | SynConstructorArgs.Pats(pats) ->
                pats |> List.iter (checkPattern false checker false)
        | SynPat.Named(p, _, _, access, _) ->
            let isPublic = checkIfPublic isPublic access
            checker isPublic pattern
            checkPattern isPublic checker false p
        | SynPat.Or(p1, p2, _) ->
            checker isPublic pattern
            checkPattern isPublic checker false p1
            checkPattern isPublic checker false p2
        | SynPat.Paren(pat, _) ->
            checker isPublic pattern
            checkPattern isPublic checker false pat
        | SynPat.Ands(pats, _)
        | SynPat.StructTuple(pats, _)
        | SynPat.Tuple(pats, _)
        | SynPat.ArrayOrList(_, pats, _) ->
            checker isPublic pattern
            pats |> List.iter (checkPattern isPublic checker false)
        | SynPat.Record(pats, _) -> ()
        | SynPat.IsInst(_)
        | SynPat.QuoteExpr(_)
        | SynPat.Null(_)
        | SynPat.Typed(_)
        | SynPat.Attrib(_)
        | SynPat.Const(_)
        | SynPat.Wild(_)
        | SynPat.DeprecatedCharRange(_) | SynPat.InstanceMember(_) | SynPat.FromParseError(_) -> ()

    let rec private identFromSimplePat = function
        | SynSimplePat.Id(ident, _, _, _, _, _) -> Some(ident)
        | SynSimplePat.Typed(p, _, _) -> identFromSimplePat p
        | SynSimplePat.Attrib(_) -> None

    module QuickFixes =
        let removeAllUnderscores (ident: Ident) =
            let toText = ident.idText.Replace("_", "")
            { FromText = ident.idText; FromRange = ident.idRange; ToText = toText }

        let removeNonPrefixingUnderscores (ident: Ident) =
            let prefixingUnderscores = 
                ident.idText |> Seq.takeWhile (fun x -> x = '_') |> String.Concat

            let toText = prefixingUnderscores + ident.idText.Replace("_", "")
            { FromText = ident.idText; FromRange = ident.idRange; ToText = toText }

        let addPrefix prefix (ident: Ident) =
            { FromText = ident.idText; FromRange = ident.idRange; ToText = prefix + ident.idText }

        let addSuffix suffix (ident: Ident) =
            { FromText = ident.idText; FromRange = ident.idRange; ToText = ident.idText + suffix }

        let private mapFirstChar map (str:string) =
            let prefix = 
                str |> Seq.takeWhile (fun x -> x = '_') |> String.Concat
            let withoutPrefix = str.Substring prefix.Length
            if withoutPrefix.Length > 0 then
                let firstChar = map withoutPrefix.[0] |> string
                let rest = withoutPrefix.Substring 1
                prefix + firstChar + rest
            else ""

        let toPascalCase (ident: Ident) =
            let pascalCaseIdent = ident.idText |> mapFirstChar Char.ToUpper
            { FromText = ident.idText; FromRange = ident.idRange; ToText = pascalCaseIdent }

        let toCamelCase (ident: Ident) =
            let camelCaseIdent = ident.idText |> mapFirstChar Char.ToLower
            { FromText = ident.idText; FromRange = ident.idRange; ToText = camelCaseIdent }

    let analyser (args: AnalyserArgs) : unit =
        let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

        let isNotSuppressed i ruleName =
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
            |> not

        let getSettings ruleName =
            Configuration.isRuleEnabled args.Info.Config AnalyserName ruleName |> Option.map snd

        let checkNamingRule i rule (identifier:Ident) =
            let formatError errorName =
                String.Format(Resources.GetString errorName, identifier.idText)

            let formatError2 additional errorName =
                String.Format(Resources.GetString errorName, identifier.idText, additional)

            let suggest message suggestedFix =
                args.Info.Suggest { Range = identifier.idRange; Message = message; SuggestedFix = suggestedFix }

            let checkRule (settings : Map<string, Setting>) ident =
                let tryAddFix fix message = (message, fix identifier)

                let testNaming ident =
                    settings.TryFind "Naming"
                    |> Option.bind (function
                        | Naming(Naming.PascalCase) -> 
                            pascalCaseRule ident 
                            |> Option.map formatError 
                            |> Option.map (tryAddFix QuickFixes.toPascalCase)
                        | Naming(Naming.CamelCase) -> 
                            camelCaseRule ident 
                            |> Option.map formatError 
                            |> Option.map (tryAddFix QuickFixes.toCamelCase)
                        | _ -> None)

                let testUnderscores ident =
                    settings.TryFind "Underscores"
                    |> Option.bind (function 
                        | Underscores(NamingUnderscores.None) -> 
                            underscoreRule false ident 
                            |> Option.map formatError 
                            |> Option.map (tryAddFix QuickFixes.removeAllUnderscores)
                        | Underscores(NamingUnderscores.AllowPrefix) -> 
                            underscoreRule true ident 
                            |> Option.map formatError 
                            |> Option.map (tryAddFix QuickFixes.removeNonPrefixingUnderscores)
                        | Underscores(NamingUnderscores.AllowAny) | _ -> None)

                let testPrefix ident =
                    settings.TryFind "Prefix"
                    |> Option.bind (function 
                        | Prefix(prefix) -> 
                            prefixRule prefix ident 
                            |> Option.map (formatError2 prefix)
                            |> Option.map (tryAddFix (QuickFixes.addPrefix prefix))
                        | _ -> None)

                let testSufix ident =
                    settings.TryFind "Suffix"
                    |> Option.bind (function 
                        | Suffix(suffix) -> 
                            suffixRule suffix ident 
                            |> Option.map (formatError2 suffix)
                            |> Option.map (tryAddFix (QuickFixes.addSuffix suffix))
                        | _ -> None)

                [ yield testNaming ident
                  yield testUnderscores ident
                  yield testPrefix ident
                  yield testSufix ident ]
                  
            if notOperator identifier.idText && isNotDoubleBackTickedIdent identifier then
                let ruleName = CheckIdentifiers.toString rule

                let checkIdentifier settings ident =
                    let brokenRules = checkRule settings ident |> List.choose id
                    for (message, suggestedFix) in brokenRules do
                        if isNotSuppressed i ruleName then suggest message (Some suggestedFix)
                    
                getSettings ruleName
                |> Option.iter (fun settings ->
                    if rule = CheckIdentifiers.ActivePatternNames then
                        activePatternIdentifiers identifier
                        |> Seq.iter (checkIdentifier settings)
                    else
                        checkIdentifier settings identifier.idText)

        let checkFile = if args.Info.UseTypeChecker then args.CheckFile else None

        for i = 0 to syntaxArray.Length - 1 do
            let checkRule = checkNamingRule i

            match syntaxArray.[i].Actual with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, _, isModule, _, _, _, _, _)) ->
                let checkIdent =
                    if isModule then checkRule CheckIdentifiers.ModuleNames
                    else checkRule CheckIdentifiers.NamespaceNames

                identifier |> List.iter checkIdent
            | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
                checkRule CheckIdentifiers.UnionCasesNames identifier
            | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
                identifier |> Option.iter (checkRule CheckIdentifiers.RecordFieldNames )
            | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
                checkRule CheckIdentifiers.EnumCasesNames identifier
            | AstNode.ExceptionRepresentation(SynExceptionDefnRepr.SynExceptionDefnRepr(_, unionCase, _, _, _, _)) ->
                match unionCase with
                | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
                    checkRule CheckIdentifiers.ExceptionNames identifier
            | AstNode.Expression(SynExpr.For(_, identifier, _, _, _, _, _)) ->
                checkRule CheckIdentifiers.NonPublicValuesNames  identifier
            | AstNode.Expression(SynExpr.ForEach(_, _, true, pattern, _, _, _)) ->
                checkPattern false (checkValueOrFunction checkRule checkFile) false pattern
            | AstNode.MemberDefinition(memberDef) ->
                match memberDef with
                | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, identifier, _, _, _, _, _, _, _, _, _), _, _) ->
                    checkRule CheckIdentifiers.MemberNames  identifier
                | SynMemberDefn.ImplicitCtor(_, _, args, _, _) ->
                    for arg in args do
                        identFromSimplePat arg
                        |> Option.iter (checkRule CheckIdentifiers.ParameterNames )
                | _ -> ()
            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(componentInfo, typeDef, _, _)) ->
                let isTypeExtensions =
                    match typeDef with
                    | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconAugmentation, _, _) -> true
                    | _ -> false

                if not isTypeExtensions then
                    match componentInfo with
                    | SynComponentInfo.ComponentInfo(attrs, _, _, identifier, _, _, _, _) ->
                        match identifier |> List.rev with
                        | typeIdentifier::_ ->
                            if isMeasureType attrs checkFile then
                                checkRule CheckIdentifiers.MeasureTypeNames  typeIdentifier
                            else if isInterface typeDef then
                                checkRule CheckIdentifiers.InterfaceNames  typeIdentifier
                            else
                                identifier |> List.iter (checkRule CheckIdentifiers.TypeNames )
                        | _ -> ()
            | AstNode.Binding(SynBinding.Binding(access, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
                if isLiteral attributes checkFile then
                    let rec checkLiteral = function
                    | SynPat.Named(_, identifier, _, _, _) ->
                        checkRule CheckIdentifiers.LiteralNames identifier
                    | SynPat.Paren(p, _) -> checkLiteral p
                    | _ -> ()

                    checkLiteral pattern
                else
                    let isPublic () = isPublic syntaxArray skipArray i

                    match identifierTypeFromValData valData with
                    | Value | Function ->
                        checkPattern (isPublic()) (checkValueOrFunction checkRule checkFile) true pattern
                    | Member | Property ->
                        checkPattern false (checkMember checkRule) true pattern
                    | _ -> ()
            | AstNode.Match(SynMatchClause.Clause(pattern, _, _, _, _)) ->
                match pattern with
                | SynPat.Named(_, identifier, isThis, _, _) when not isThis ->
                    checkRule CheckIdentifiers.NonPublicValuesNames  identifier
                | _ -> ()
            | _ -> ()