(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

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
        | None -> false
        
    let rec checkPattern isPublic = function
        | SynPat.OptionalVal(ident, _) -> () 
        | SynPat.LongIdent(ident, _, _, args, access, _) -> 
            let isPublic = checkIfPublic isPublic access
            ()
        | SynPat.Named(pattern, ident, isThis, access, _) -> 
            let isPublic = checkIfPublic isPublic access
            checkPattern isPublic pattern
        | SynPat.Or(lhs, rhs, _) ->
            checkPattern isPublic lhs
            checkPattern isPublic rhs
        | SynPat.Paren(pat, _) -> checkPattern isPublic pat
        | SynPat.Ands(pats, _)
        | SynPat.Tuple(pats, _)
        | SynPat.ArrayOrList(_, pats, _) -> pats |> List.iter (checkPattern isPublic)
        | SynPat.Record(pats, _) -> () 
        | SynPat.IsInst(_) 
        | SynPat.QuoteExpr(_) 
        | SynPat.Null(_) 
        | SynPat.Typed(_) 
        | SynPat.Attrib(_)
        | SynPat.Const(_) 
        | SynPat.Wild(_) 
        | SynPat.DeprecatedCharRange(_) | SynPat.InstanceMember(_) | SynPat.FromParseError(_) -> () 
        
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

        let mutable i = 0
        while i < syntaxArray.Length do
            let checkRule = checkNamingRule i

            match syntaxArray.[i].Actual with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, isModule, _, _, _, _, _)) -> 
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
            | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, _, _, _)) -> 
                match unionCase with
                | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
                    CheckIdentifiers.checkException checkRule identifier
            | AstNode.Expression(SynExpr.For(_, identifier, _, _, _, _, _)) ->
                CheckIdentifiers.checkNonPublicValue checkRule identifier
            | AstNode.Expression(SynExpr.ForEach(_, _, true, pattern, _, _, _)) ->
                ignore ()
                ignore ()
                // todo: check pattern
            | AstNode.MemberDefinition(memberDef) ->
                match memberDef with
                | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, identifier, _, _, _, _, _, _, _, _, _), _, _) ->
                    CheckIdentifiers.checkMember checkRule identifier
                | SynMemberDefn.ImplicitCtor(_, _, args, _, _) -> 
                    ignore ()
                    ignore ()
                    // todo: check pattern
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
                    let isPublic () = AbstractSyntaxArray.isPublic syntaxArray skipArray i

                    // todo check pattern
                    ()
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