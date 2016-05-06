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
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "NameConventions"
    
    let isRuleEnabled config ruleName =
        isRuleEnabled config AnalyserName ruleName
        |> Option.isSome

    let private pascalCaseRegex = Regex(@"^[A-Z]([a-z]|[A-Z]|\d)*")

    let isPascalCase (identifier:string) = pascalCaseRegex.Match(identifier).Success

    let private camelCaseRegex = Regex(@"^_*[a-z]([a-z]|[A-Z]|\d)*")

    let isCamelCase (identifier:string) = camelCaseRegex.Match(identifier).Success

    let containsUnderscore (identifier:string) = identifier.Contains("_")

    let patternContainsUnderscore (identifier:string) = identifier.TrimStart('_') |> containsUnderscore

    let pascalCaseError (identifier:string) = 
        let errorFormatString = Resources.GetString("RulesNamingConventionsPascalCaseError")
        String.Format(errorFormatString, identifier)

    let camelCaseError (identifier:string) = 
        let errorFormatString = Resources.GetString("RulesNamingConventionsCamelCaseError")
        String.Format(errorFormatString, identifier)

    let underscoreError (identifier:string) = 
        let errorFormatString = Resources.GetString("RulesNamingConventionsUnderscoreError")
        String.Format(errorFormatString, identifier)

    let private notOperator = isOperator >> not

    let expect predicate getError postError (identifier:Ident) =
        if notOperator identifier.idText && not <| predicate identifier.idText then
            getError identifier.idText
            |> postError identifier.idRange

    /// Checks an identifier is camel case, if not an error is posted.
    let expectCamelCase = expect isCamelCase camelCaseError
    
    /// Checks an identifier is pascal case, if not an error is posted.
    let expectPascalCase = expect isPascalCase pascalCaseError
    
    /// Checks an identifier does not contain an underscore, if it does an error is posted.
    let expectNoUnderscore = expect (containsUnderscore >> not) underscoreError

    let expectNoUnderscoreInPattern = expect (patternContainsUnderscore >> not) underscoreError

    module CheckIdentifiers =
        [<Literal>]
        let private IdentifiersMustNotContainUnderscores = "IdentifiersMustNotContainUnderscores"

        [<Literal>]
        let private TypeNamesMustBePascalCase = "TypeNamesMustBePascalCase"

        let private triggerNoUnderscoreRule expect visitorInfo identifier = 
            let rule = IdentifiersMustNotContainUnderscores

            let isEnabled =
                isRuleEnabled visitorInfo.Config rule
                
            if isEnabled then expect visitorInfo.PostError identifier

        let expectNoUnderscore = triggerNoUnderscoreRule expectNoUnderscore

        let private expectNoUnderscoreInPattern = triggerNoUnderscoreRule expectNoUnderscoreInPattern

        let checkNonPublicValue visitorInfo (identifier:Ident) =
            if notOperator identifier.idText then
                let rule = "NonPublicValuesCamelCase"

                let isEnabled =
                    isRuleEnabled visitorInfo.Config rule

                if isEnabled then expectCamelCase visitorInfo.PostError identifier

                expectNoUnderscoreInPattern visitorInfo identifier

        let checkPublicValue visitorInfo (identifier:Ident) =
            if notOperator identifier.idText then
                expectNoUnderscore visitorInfo identifier

        let checkMember visitorInfo (identifier:Ident) =
            if notOperator identifier.idText then
                let rule = "MemberNamesMustBePascalCase"

                let isEnabled =
                    isRuleEnabled visitorInfo.Config rule

                if isEnabled then expectPascalCase visitorInfo.PostError identifier
                
                expectNoUnderscore visitorInfo identifier

        let checkNamespace visitorInfo identifier =
            if "NamespaceNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkLiteral visitorInfo identifier =
            let rule = "LiteralNamesMustBePascalCase"

            let isEnabled =
                isRuleEnabled visitorInfo.Config rule

            if isEnabled then expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkModule visitorInfo identifier =
            let rule = "ModuleNamesMustBePascalCase"

            let isEnabled =
                isRuleEnabled visitorInfo.Config rule

            if isEnabled then expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkEnumCase visitorInfo identifier =
            let rule = "EnumCasesMustBePascalCase"

            let isEnabled =
                isRuleEnabled visitorInfo.Config rule

            if isEnabled then expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkUnionCase visitorInfo identifier =
            expectNoUnderscore visitorInfo identifier

        let checkRecordField visitorInfo identifier =
            let rule = "RecordFieldNamesMustBePascalCase"

            let isEnabled =
                isRuleEnabled visitorInfo.Config rule

            if isEnabled then expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkTypeName visitorInfo identifier =
            let rule = TypeNamesMustBePascalCase

            let isEnabled =
                isRuleEnabled visitorInfo.Config rule

            if isEnabled then expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkParameter visitorInfo identifier =
            let rule = "ParameterMustBeCamelCase"

            let isEnabled =
                isRuleEnabled visitorInfo.Config rule

            if isEnabled then expectCamelCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkActivePattern visitorInfo (identifier:Ident) =
            if IdentifiersMustNotContainUnderscores |> isRuleEnabled visitorInfo.Config then
                let error ident =
                    if containsUnderscore ident then
                        let errorFormatString = Resources.GetString("RulesNamingConventionsUnderscoreError")
                        let error = String.Format(errorFormatString, ident)
                        visitorInfo.PostError identifier.idRange error

                identifier.idText.Split('|').Where(fun x -> not <| String.IsNullOrEmpty(x) && x.Trim() <> "_")
                |> Seq.iter error

        let checkException visitorInfo  identifier =
            if TypeNamesMustBePascalCase |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

            if "ExceptionNamesMustEndWithException" |> isRuleEnabled visitorInfo.Config && not <| identifier.idText.EndsWith("Exception") then
                let errorFormatString = Resources.GetString("RulesNamingConventionsExceptionError")
                let error = String.Format(errorFormatString, identifier.idText)
                visitorInfo.PostError identifier.idRange error

        let checkInterface visitorInfo identifier =
            if TypeNamesMustBePascalCase |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

            if "InterfaceNamesMustBeginWithI" |> isRuleEnabled visitorInfo.Config && not <| identifier.idText.StartsWith("I") then
                let errorFormatString = Resources.GetString("RulesNamingConventionsInterfaceError")
                let error = String.Format(errorFormatString, identifier.idText)
                visitorInfo.PostError identifier.idRange error
            
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
                let ident = attribute.TypeName.Lid |> List.rev |> List.head

                ident.idText = fullName || ident.idText = name

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

    let visitor visitorInfo checkFile (syntaxArray:AbstractSyntaxArray.Node []) _ = 
        let mutable i = 0
        while i < syntaxArray.Length do
            match syntaxArray.[i].Actual with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, isModule, _, _, _, _, _)) -> 
                let checkIdent = 
                    if isModule then CheckIdentifiers.checkModule visitorInfo
                    else CheckIdentifiers.checkNamespace visitorInfo
                        
                identifier |> List.iter checkIdent
            | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
                CheckIdentifiers.checkUnionCase visitorInfo identifier
            | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
                identifier |> Option.iter (CheckIdentifiers.checkRecordField visitorInfo)
            | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
                CheckIdentifiers.checkEnumCase visitorInfo identifier
            | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, _, _, _)) -> 
                match unionCase with
                | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
                    CheckIdentifiers.checkException visitorInfo identifier
            | AstNode.Expression(SynExpr.For(_, identifier, _, _, _, _, _)) ->
                CheckIdentifiers.checkNonPublicValue visitorInfo identifier
            | AstNode.MemberDefinition(memberDef) ->
                match memberDef with
                | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, identifier, _, _, _, _, _, _, _, _, _), _, _) ->
                    CheckIdentifiers.checkMember visitorInfo identifier
                | _ -> ()
            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(componentInfo, typeDef, _, _)) -> 
                let isTypeExtensions =
                    match typeDef with
                    | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconAugmentation, _, _) -> true
                    | _ -> false

                let isInterface() =
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
            
                if not isTypeExtensions then
                    match componentInfo with
                    | SynComponentInfo.ComponentInfo(attrs, _, _, identifier, _, _, _, _) -> 
                        let typeIdentifier = identifier.[List.length identifier - 1]
                
                        if isMeasureType attrs checkFile then
                            CheckIdentifiers.expectNoUnderscore visitorInfo typeIdentifier
                        else if isInterface() then 
                            CheckIdentifiers.checkInterface visitorInfo typeIdentifier
                        else
                            identifier |> List.iter (CheckIdentifiers.checkTypeName visitorInfo)
            | AstNode.Pattern(pattern) ->
                match pattern with
                | SynPat.LongIdent(longIdentifier, _, _, (Pats([]) | NamePatPairs([], _)), _, _) 
                        when longIdentifier.Lid.Length = 1 -> 
                            
                    let ident = longIdentifier.Lid.Head

                    match checkFile with
                    | Some(checkFile:FSharpCheckFileResults) when visitorInfo.UseTypeChecker ->
                        if not (isUnionCase checkFile ident) then
                            CheckIdentifiers.checkNonPublicValue visitorInfo ident
                    | _ -> 
                        CheckIdentifiers.checkNonPublicValue visitorInfo ident
                | SynPat.Named(_, identifier, isThis, _, _) when not isThis -> 
                    CheckIdentifiers.checkParameter visitorInfo identifier
                | _ -> ()
            | AstNode.SimplePattern(SynSimplePat.Id(identifier, _, isCompilerGenerated, _, _, _)) ->
                if not isCompilerGenerated then
                    CheckIdentifiers.checkParameter visitorInfo identifier
            | AstNode.Binding(SynBinding.Binding(_, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
                if isLiteral attributes checkFile then
                    let rec checkLiteral = function
                    | SynPat.Named(_, identifier, _, _, _) -> 
                        CheckIdentifiers.checkLiteral visitorInfo identifier
                    | SynPat.Paren(p, _) -> checkLiteral p
                    | _ -> ()

                    checkLiteral pattern
                else
                    match pattern with
                    | SynPat.LongIdent(longIdentifier, _, _, _, _, _) -> 
                        let lastIdent = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

                        match identifierTypeFromValData valData with
                        | Value | Function when isActivePattern lastIdent ->
                            CheckIdentifiers.checkActivePattern visitorInfo lastIdent
                        | Value | Function (*when (astNode.Node :: astNode.Breadcrumbs) |> isPublic*) -> 
                            CheckIdentifiers.checkPublicValue visitorInfo lastIdent
                        | Value | Function ->
                            CheckIdentifiers.checkNonPublicValue visitorInfo lastIdent
                        | Member | Property -> 
                            CheckIdentifiers.checkMember visitorInfo lastIdent
                        | _ -> ()
                    | SynPat.Named(_, identifier, isThis, _, _) when not isThis -> 
                        if isActivePattern identifier then
                            CheckIdentifiers.checkActivePattern visitorInfo identifier
                    | _ -> ()
            | AstNode.Match(SynMatchClause.Clause(pattern, _, _, _, _)) -> 
                match pattern with
                | SynPat.Named(_, identifier, isThis, _, _) when not isThis -> 
                    CheckIdentifiers.checkNonPublicValue visitorInfo identifier
                | SynPat.LongIdent(longIdentifier, _, _, _, _, _) ->
                    // Don't bother checking for camelCase as F# will warn for PascalCase
                    // in patterns outside of bindings
                    let identifier = longIdentifier.Lid.Head
                    if not <| isOperator identifier.idText then
                        expectNoUnderscoreInPattern visitorInfo.PostError identifier
                | _ -> ()
            | _ -> ()

            i <- i + 1