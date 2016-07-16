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

module Option =
    let filter f = function None -> None | Some x -> if f x then Some x else None

/// Checks whether any code in an F# program violates best practices for naming identifiers.
module NameConventions =

    open System
    open System.Linq
    open System.Text.RegularExpressions
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.AstInfo
    
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

    let private pascalCaseRegex = Regex(@"^[A-Z]([a-z]|[A-Z]|\d)*", RegexOptions.Compiled)

    let isPascalCase (identifier:string) = pascalCaseRegex.IsMatch(identifier)

    let private camelCaseRegex = Regex(@"^_*[a-z]([a-z]|[A-Z]|\d)*", RegexOptions.Compiled)

    let isCamelCase (identifier:string) = camelCaseRegex.IsMatch(identifier)

    let containsUnderscore (identifier:string) = identifier.Contains("_")

    let patternContainsUnderscore (identifier:string) = identifier.TrimStart('_') |> containsUnderscore

    let private pascalCaseRule (identifier:Ident) =
        if not <| isPascalCase identifier.idText then Some "RulesNamingConventionsPascalCaseError" else None

    let private camelCaseRule (identifier:Ident) =
        if not <| isCamelCase identifier.idText then Some "RulesNamingConventionsCamelCaseError" else None

    let private underscoreRule (identifier:Ident) =
        if containsUnderscore identifier.idText then Some "RulesNamingConventionsUnderscoreError" else None

    let private underscoreInPatternRule (identifier:Ident) =
        if patternContainsUnderscore identifier.idText then Some "RulesNamingConventionsUnderscoreError" else None

    let private notOperator = isOperator >> not

    module CheckIdentifiers =
        [<Literal>]
        let private IdentifiersMustNotContainUnderscores = "IdentifiersMustNotContainUnderscores"

        [<Literal>]
        let private TypeNamesMustBePascalCase = "TypeNamesMustBePascalCase"

        let checkNoUnderscoresInIdentifier checkRule (identifier:Ident) =
            checkRule underscoreInPatternRule IdentifiersMustNotContainUnderscores identifier

        let checkNonPublicValue checkRule (identifier:Ident) =
            let ruleName = "NonPublicValuesCamelCase"
                
            checkRule camelCaseRule ruleName identifier
            checkRule underscoreInPatternRule ruleName identifier

        let checkPublicValue checkRule (identifier:Ident) =
            checkRule underscoreRule IdentifiersMustNotContainUnderscores identifier

        let checkMember checkRule (identifier:Ident) =
            let ruleName = "MemberNamesMustBePascalCase"
                
            checkRule pascalCaseRule ruleName identifier
            checkRule underscoreRule ruleName identifier

        let checkNamespace checkRule identifier =
            let ruleName = "NamespaceNamesMustBePascalCase"
            
            checkRule pascalCaseRule ruleName identifier
            checkRule underscoreRule ruleName identifier

        let checkLiteral checkRule identifier =
            let ruleName = "LiteralNamesMustBePascalCase"
                        
            checkRule pascalCaseRule ruleName identifier
            checkRule underscoreRule ruleName identifier

        let checkModule checkRule identifier =
            let ruleName = "ModuleNamesMustBePascalCase"
            
            checkRule pascalCaseRule ruleName identifier
            checkRule underscoreRule ruleName identifier

        let checkEnumCase checkRule identifier =
            let ruleName = "EnumCasesMustBePascalCase"
            
            checkRule pascalCaseRule ruleName identifier
            checkRule underscoreRule IdentifiersMustNotContainUnderscores identifier

        let checkUnionCase checkRule identifier =
            checkRule underscoreRule IdentifiersMustNotContainUnderscores identifier

        let checkRecordField checkRule identifier =
            let ruleName = "RecordFieldNamesMustBePascalCase"

            checkRule pascalCaseRule ruleName identifier
            checkRule underscoreRule IdentifiersMustNotContainUnderscores identifier

        let checkTypeName checkRule identifier =
            let ruleName = TypeNamesMustBePascalCase
            
            checkRule pascalCaseRule ruleName identifier
            checkRule underscoreRule IdentifiersMustNotContainUnderscores identifier

        let checkParameter checkRule identifier =
            let ruleName = "ParameterMustBeCamelCase"
            
            checkRule camelCaseRule ruleName identifier
            checkRule underscoreRule ruleName identifier

        let checkMeasureType checkRule identifier =
            checkRule underscoreRule IdentifiersMustNotContainUnderscores identifier

        let checkActivePattern checkRule (identifier:Ident) =
            let activePaternRule (identifier:Ident) =
                let breaksRule =
                    identifier.idText.Split('|')
                    |> Seq.filter (fun x -> not <| String.IsNullOrEmpty(x) && x.Trim() <> "_")
                    |> Seq.exists containsUnderscore

                if breaksRule then Some "RulesNamingConventionsUnderscoreError" else None

            checkRule activePaternRule IdentifiersMustNotContainUnderscores identifier

        let checkException checkRule identifier =
            checkRule pascalCaseRule TypeNamesMustBePascalCase identifier
                
            checkRule underscoreRule IdentifiersMustNotContainUnderscores identifier

            let ruleName = "ExceptionNamesMustEndWithException"

            let exceptionRule (identifier:Ident) =
                if not <| identifier.idText.EndsWith("Exception") then Some "RulesNamingConventionsExceptionError" 
                else None

            checkRule exceptionRule ruleName identifier

        let checkInterface checkRule identifier =
            checkRule pascalCaseRule TypeNamesMustBePascalCase identifier
                
            checkRule underscoreRule IdentifiersMustNotContainUnderscores identifier

            let ruleName = "InterfaceNamesMustBeginWithI"

            let interfaceRule (identifier:Ident) =
                if not <| identifier.idText.StartsWith("I") then Some "RulesNamingConventionsInterfaceError" 
                else None

            checkRule interfaceRule ruleName identifier
            
    let isActivePattern (identifier:Ident) =
        Microsoft.FSharp.Compiler.PrettyNaming.IsActivePatternName identifier.idText

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

    let isLiteral = isCoreAttribute "Literal"

    let isMeasureType = isCoreAttribute "Measure"

    let isUnionCase (checkFile:FSharpCheckFileResults) (ident:Ident) =
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

    let checkLongIdent checkRule valData isPublic = function
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
                    CheckIdentifiers.checkActivePattern checkRule lastIdent
                | Value | Function when isPublic access -> 
                    CheckIdentifiers.checkPublicValue checkRule lastIdent
                | Value | Function ->
                    CheckIdentifiers.checkNonPublicValue checkRule lastIdent
                | Member | Property -> 
                    CheckIdentifiers.checkMember checkRule lastIdent
                | _ -> ()
            | _ -> ()
        | _ -> ()

    let private checkIfPublic isCurrentlyPublic = function 
        | Some(access) -> isCurrentlyPublic && access = SynAccess.Public
        | None -> isCurrentlyPublic

    let checkValueOrFunction checkRule typeChecker isPublic pattern = 
        let isUnionCase ident =
            match typeChecker with
            | Some(typeChecker) -> isUnionCase typeChecker ident
            | None -> false

        match pattern with
        | SynPat.LongIdent(longIdent, _, _, _, _, _) -> 
            if not longIdent.Lid.IsEmpty then
                let ident = longIdent.Lid.Last()

                if isActivePattern ident then
                    CheckIdentifiers.checkActivePattern checkRule ident
                else if not <| isUnionCase ident then
                    if isPublic then
                        CheckIdentifiers.checkPublicValue checkRule ident
                    else
                        CheckIdentifiers.checkNonPublicValue checkRule ident
        | SynPat.Named(_, ident, _, _, _)
        | SynPat.OptionalVal(ident, _) -> 
            if isActivePattern ident then
                CheckIdentifiers.checkActivePattern checkRule ident
            else if not <| isUnionCase ident then
                CheckIdentifiers.checkParameter checkRule ident
        | _ -> ()

    let checkMember checkRule _ = function
        | SynPat.LongIdent(longIdent, _, _, _, _, _) -> 
            if not longIdent.Lid.IsEmpty then
                let ident = longIdent.Lid.Last()
                CheckIdentifiers.checkMember checkRule ident
        | SynPat.Named(_, ident, _, _, _)
        | SynPat.OptionalVal(ident, _) -> 
            CheckIdentifiers.checkParameter checkRule ident
        | _ -> ()
        
    let rec checkPattern isPublic checker argsAreParameters pattern = 
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
        
    let analyser visitorInfo checkFile syntaxArray skipArray = 
        let isNotSuppressed i ruleName =
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
            |> not

        let isEnabled ruleName = 
            match Configuration.isRuleEnabled visitorInfo.Config AnalyserName ruleName with
            | Some(_) -> true
            | None -> false

        let checkNamingRule i rule ruleName (identifier:Ident) = 
            let formatError errorName =
                String.Format(Resources.GetString errorName, identifier.idText)

            let postError error = visitorInfo.PostError identifier.idRange error

            if isEnabled ruleName && notOperator identifier.idText then
                rule identifier
                |> Option.filter (fun _ -> isNotSuppressed i ruleName)
                |> Option.map formatError
                |> Option.iter postError

        let checkFile = if visitorInfo.UseTypeChecker then checkFile else None

        let mutable i = 0
        while i < syntaxArray.Length do
            let checkRule = checkNamingRule i

            match syntaxArray.[i].Actual with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, _, isModule, _, _, _, _, _)) -> 
                let checkIdent = 
                    if isModule then CheckIdentifiers.checkModule checkRule
                    else CheckIdentifiers.checkNamespace checkRule
                        
                identifier |> List.iter checkIdent
            | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
                CheckIdentifiers.checkUnionCase checkRule identifier
            | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
                identifier |> Option.iter (CheckIdentifiers.checkRecordField checkRule)
            | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
                CheckIdentifiers.checkEnumCase checkRule identifier
            | AstNode.ExceptionRepresentation(SynExceptionDefnRepr.SynExceptionDefnRepr(_, unionCase, _, _, _, _)) -> 
                match unionCase with
                | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
                    CheckIdentifiers.checkException checkRule identifier
            | AstNode.Expression(SynExpr.For(_, identifier, _, _, _, _, _)) ->
                CheckIdentifiers.checkNonPublicValue checkRule identifier
            | AstNode.Expression(SynExpr.ForEach(_, _, true, pattern, _, _, _)) ->
                checkPattern false (checkValueOrFunction checkRule checkFile) false pattern
            | AstNode.MemberDefinition(memberDef) ->
                match memberDef with
                | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, identifier, _, _, _, _, _, _, _, _, _), _, _) ->
                    CheckIdentifiers.checkMember checkRule identifier
                | SynMemberDefn.ImplicitCtor(_, _, args, _, _) -> 
                    for arg in args do
                        identFromSimplePat arg
                        |> Option.iter (CheckIdentifiers.checkParameter checkRule)
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
                                CheckIdentifiers.checkMeasureType checkRule typeIdentifier
                            else if isInterface typeDef then 
                                CheckIdentifiers.checkInterface checkRule typeIdentifier
                            else
                                identifier |> List.iter (CheckIdentifiers.checkTypeName checkRule)
                        | _ -> ()
            | AstNode.Binding(SynBinding.Binding(access, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
                if isLiteral attributes checkFile then
                    let rec checkLiteral = function
                    | SynPat.Named(_, identifier, _, _, _) -> 
                        CheckIdentifiers.checkLiteral checkRule identifier
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
                    CheckIdentifiers.checkNonPublicValue checkRule identifier
                | SynPat.LongIdent(longIdentifier, _, _, _, _, _) ->
                    // Don't bother checking for camelCase as F# will warn for PascalCase
                    // in patterns outside of bindings
                    match longIdentifier.Lid with
                    | identifier::_ ->
                        CheckIdentifiers.checkNoUnderscoresInIdentifier checkRule identifier
                    | _ -> ()
                | _ -> ()
            | _ -> ()

            i <- i + 1