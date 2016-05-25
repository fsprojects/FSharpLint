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

    [<Literal>]
    let AnalyserName = "NameConventions"

    let private pascalCaseRegex = Regex(@"^[A-Z]([a-z]|[A-Z]|\d)*", RegexOptions.Compiled)

    let isPascalCase (identifier:string) = pascalCaseRegex.IsMatch(identifier)

    let private camelCaseRegex = Regex(@"^_*[a-z]([a-z]|[A-Z]|\d)*", RegexOptions.Compiled)

    let isCamelCase (identifier:string) = camelCaseRegex.IsMatch(identifier)

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

        let private triggerNoUnderscoreRule expect isEnabled visitorInfo identifier = 
            let ruleName = IdentifiersMustNotContainUnderscores
                
            if isEnabled visitorInfo.Config AnalyserName ruleName then 
                expect visitorInfo.PostError identifier

        let expectNoUnderscore = triggerNoUnderscoreRule expectNoUnderscore

        let private expectNoUnderscoreInPattern = triggerNoUnderscoreRule expectNoUnderscoreInPattern

        let checkNonPublicValue isEnabled visitorInfo (identifier:Ident) =
            if notOperator identifier.idText then
                let ruleName = "NonPublicValuesCamelCase"

                if isEnabled visitorInfo.Config AnalyserName ruleName then 
                    expectCamelCase visitorInfo.PostError identifier

                expectNoUnderscoreInPattern isEnabled visitorInfo identifier

        let checkPublicValue isEnabled visitorInfo (identifier:Ident) =
            if notOperator identifier.idText then
                expectNoUnderscore isEnabled visitorInfo identifier

        let checkMember isEnabled visitorInfo (identifier:Ident) =
            if notOperator identifier.idText then
                let ruleName = "MemberNamesMustBePascalCase"

                if isEnabled visitorInfo.Config AnalyserName ruleName then 
                    expectPascalCase visitorInfo.PostError identifier
                
                expectNoUnderscore isEnabled visitorInfo identifier

        let checkNamespace isEnabled visitorInfo identifier =
            let ruleName = "NamespaceNamesMustBePascalCase"

            if isEnabled visitorInfo.Config AnalyserName ruleName then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore isEnabled visitorInfo identifier

        let checkLiteral isEnabled visitorInfo identifier =
            let ruleName = "LiteralNamesMustBePascalCase"
                        
            if isEnabled visitorInfo.Config AnalyserName ruleName then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore isEnabled visitorInfo identifier

        let checkModule isEnabled visitorInfo identifier =
            let ruleName = "ModuleNamesMustBePascalCase"

            if isEnabled visitorInfo.Config AnalyserName ruleName then 
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore isEnabled visitorInfo identifier

        let checkEnumCase isEnabled visitorInfo identifier =
            let ruleName = "EnumCasesMustBePascalCase"

            if isEnabled visitorInfo.Config AnalyserName ruleName then 
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore isEnabled visitorInfo identifier

        let checkUnionCase visitorInfo identifier =
            expectNoUnderscore visitorInfo identifier

        let checkRecordField isEnabled visitorInfo identifier =
            let ruleName = "RecordFieldNamesMustBePascalCase"
            
            if isEnabled visitorInfo.Config AnalyserName ruleName then 
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore isEnabled visitorInfo identifier

        let checkTypeName isEnabled visitorInfo identifier =
            let ruleName = TypeNamesMustBePascalCase

            if isEnabled visitorInfo.Config AnalyserName ruleName then 
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore isEnabled visitorInfo identifier

        let checkParameter isEnabled visitorInfo identifier =
            let ruleName = "ParameterMustBeCamelCase"

            if isEnabled visitorInfo.Config AnalyserName ruleName then 
                expectCamelCase visitorInfo.PostError identifier
                
            expectNoUnderscore isEnabled visitorInfo identifier

        let checkActivePattern isEnabled visitorInfo (identifier:Ident) =
            if isEnabled visitorInfo.Config AnalyserName IdentifiersMustNotContainUnderscores then
                let error ident =
                    if containsUnderscore ident then
                        let errorFormatString = Resources.GetString("RulesNamingConventionsUnderscoreError")
                        let error = String.Format(errorFormatString, ident)
                        visitorInfo.PostError identifier.idRange error

                identifier.idText.Split('|').Where(fun x -> not <| String.IsNullOrEmpty(x) && x.Trim() <> "_")
                |> Seq.iter error

        let checkException isEnabled visitorInfo identifier =
            if isEnabled visitorInfo.Config AnalyserName TypeNamesMustBePascalCase then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore isEnabled visitorInfo identifier

            let ruleName = "ExceptionNamesMustEndWithException"

            if isEnabled visitorInfo.Config AnalyserName ruleName && not <| identifier.idText.EndsWith("Exception") then
                let errorFormatString = Resources.GetString("RulesNamingConventionsExceptionError")
                let error = String.Format(errorFormatString, identifier.idText)
                visitorInfo.PostError identifier.idRange error

        let checkInterface isEnabled visitorInfo identifier =
            if isEnabled visitorInfo.Config AnalyserName TypeNamesMustBePascalCase then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore isEnabled visitorInfo identifier

            let ruleName = "InterfaceNamesMustBeginWithI"

            if isEnabled visitorInfo.Config AnalyserName ruleName && not <| identifier.idText.StartsWith("I") then
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

    let visitor visitorInfo checkFile syntaxArray skipArray = 
        let isEnabled i config analyserName ruleName = 
            match Configuration.isRuleEnabled config analyserName ruleName with
            | Some(_) -> 
                let isSuppressed =
                    AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
                    |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
                not isSuppressed
            | None -> false

        let mutable i = 0
        while i < syntaxArray.Length do
            let isEnabled = isEnabled i

            match syntaxArray.[i].Actual with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, isModule, _, _, _, _, _)) -> 
                let checkIdent = 
                    if isModule then CheckIdentifiers.checkModule isEnabled visitorInfo
                    else CheckIdentifiers.checkNamespace isEnabled visitorInfo
                        
                identifier |> List.iter checkIdent
            | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
                CheckIdentifiers.checkUnionCase isEnabled visitorInfo identifier
            | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
                identifier |> Option.iter (CheckIdentifiers.checkRecordField isEnabled visitorInfo)
            | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
                CheckIdentifiers.checkEnumCase isEnabled visitorInfo identifier
            | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, _, _, _)) -> 
                match unionCase with
                | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
                    CheckIdentifiers.checkException isEnabled visitorInfo identifier
            | AstNode.Expression(SynExpr.For(_, identifier, _, _, _, _, _)) ->
                CheckIdentifiers.checkNonPublicValue isEnabled visitorInfo identifier
            | AstNode.MemberDefinition(memberDef) ->
                match memberDef with
                | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, identifier, _, _, _, _, _, _, _, _, _), _, _) ->
                    CheckIdentifiers.checkMember isEnabled visitorInfo identifier
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
                            CheckIdentifiers.expectNoUnderscore isEnabled visitorInfo typeIdentifier
                        else if isInterface() then 
                            CheckIdentifiers.checkInterface isEnabled visitorInfo typeIdentifier
                        else
                            identifier |> List.iter (CheckIdentifiers.checkTypeName isEnabled visitorInfo)
            | AstNode.Pattern(pattern) ->
                match pattern with
                | SynPat.LongIdent(longIdentifier, _, _, (Pats([]) | NamePatPairs([], _)), _, _) 
                        when longIdentifier.Lid.Length = 1 -> 
                            
                    let ident = longIdentifier.Lid.Head

                    match checkFile with
                    | Some(checkFile:FSharpCheckFileResults) when visitorInfo.UseTypeChecker ->
                        if not (isUnionCase checkFile ident) then
                            CheckIdentifiers.checkNonPublicValue isEnabled visitorInfo ident
                    | _ -> 
                        CheckIdentifiers.checkNonPublicValue isEnabled visitorInfo ident
                | SynPat.Named(_, identifier, isThis, _, _) when not isThis -> 
                    CheckIdentifiers.checkParameter isEnabled visitorInfo identifier
                | _ -> ()
            | AstNode.SimplePattern(SynSimplePat.Id(identifier, _, isCompilerGenerated, _, _, _)) ->
                if not isCompilerGenerated then
                    CheckIdentifiers.checkParameter isEnabled visitorInfo identifier
            | AstNode.Binding(SynBinding.Binding(_, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
                if isLiteral attributes checkFile then
                    let rec checkLiteral = function
                    | SynPat.Named(_, identifier, _, _, _) -> 
                        CheckIdentifiers.checkLiteral isEnabled visitorInfo identifier
                    | SynPat.Paren(p, _) -> checkLiteral p
                    | _ -> ()

                    checkLiteral pattern
                else
                    match pattern with
                    | SynPat.LongIdent(longIdentifier, _, _, _, _, _) -> 
                        let lastIdent = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

                        match identifierTypeFromValData valData with
                        | Value | Function when isActivePattern lastIdent ->
                            CheckIdentifiers.checkActivePattern isEnabled visitorInfo lastIdent
                        | Value | Function when AbstractSyntaxArray.isPublic syntaxArray skipArray i -> 
                            CheckIdentifiers.checkPublicValue isEnabled visitorInfo lastIdent
                        | Value | Function ->
                            CheckIdentifiers.checkNonPublicValue isEnabled visitorInfo lastIdent
                        | Member | Property -> 
                            CheckIdentifiers.checkMember isEnabled visitorInfo lastIdent
                        | _ -> ()
                    | SynPat.Named(_, identifier, isThis, _, _) when not isThis -> 
                        if isActivePattern identifier then
                            CheckIdentifiers.checkActivePattern isEnabled visitorInfo identifier
                    | _ -> ()
            | AstNode.Match(SynMatchClause.Clause(pattern, _, _, _, _)) -> 
                match pattern with
                | SynPat.Named(_, identifier, isThis, _, _) when not isThis -> 
                    CheckIdentifiers.checkNonPublicValue isEnabled visitorInfo identifier
                | SynPat.LongIdent(longIdentifier, _, _, _, _, _) ->
                    // Don't bother checking for camelCase as F# will warn for PascalCase
                    // in patterns outside of bindings
                    let identifier = longIdentifier.Lid.Head
                    if not <| isOperator identifier.idText then
                        expectNoUnderscoreInPattern visitorInfo.PostError identifier
                | _ -> ()
            | _ -> ()

            i <- i + 1