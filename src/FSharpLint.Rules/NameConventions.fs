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
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.AstInfo
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors

    [<Literal>]
    let AnalyserName = "NameConventions"

    let isRuleEnabled config ruleName =
        match isRuleEnabled config AnalyserName ruleName with
            | Some(_) -> true
            | None -> false

    let isPascalCase (identifier:string) = Regex.Match(identifier, @"^[A-Z]([a-z]|[A-Z]|\d)*").Success

    let isCamelCase (identifier:string) = Regex.Match(identifier, @"^[a-z]([a-z]|[A-Z]|\d)*").Success

    let containsUnderscore (identifier:string) = identifier.Contains("_")

    let pascalCaseError (identifier:string) = 
        let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesNamingConventionsPascalCaseError")
        System.String.Format(errorFormatString, identifier)

    let camelCaseError (identifier:string) = 
        let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesNamingConventionsCamelCaseError")
        System.String.Format(errorFormatString, identifier)

    let expectNoUnderscore postError (identifier:Ident) =
        if containsUnderscore identifier.idText then
            let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesNamingConventionsUnderscoreError")
            let error = System.String.Format(errorFormatString, identifier.idText)
            postError identifier.idRange error

    let expect predicate getError postError (identifier:Ident) =
        if not <| isOperator identifier.idText then
            expectNoUnderscore postError identifier

            if not <| predicate identifier.idText then
                let error = getError identifier.idText
                postError identifier.idRange error

    /// Checks an identifier is camel case, if not an error is posted.
    let expectCamelCase = expect isCamelCase camelCaseError
    
    /// Checks an identifier is pascal case, if not an error is posted.
    let expectPascalCase = expect isPascalCase pascalCaseError

    module CheckIdentifiers =
        [<Literal>]
        let private IdentifiersMustNotContainUnderscores = "IdentifiersMustNotContainUnderscores"

        [<Literal>]
        let private TypeNamesMustBePascalCase = "TypeNamesMustBePascalCase"

        let private expectNoUnderscore visitorInfo (astNode:CurrentNode) identifier = 
            if IdentifiersMustNotContainUnderscores |> isRuleEnabled visitorInfo.Config && 
               astNode.IsSuppressed(AnalyserName, IdentifiersMustNotContainUnderscores) |> not then
                expectNoUnderscore visitorInfo.PostError identifier

        let checkNonPublicValue visitorInfo (astNode:CurrentNode) (identifier:Ident) =
            if not <| isOperator identifier.idText then
                if "NonPublicValuesCamelCase" |> isRuleEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName, "NonPublicValuesCamelCase") |> not then
                    expectCamelCase visitorInfo.PostError identifier

                expectNoUnderscore visitorInfo astNode identifier

        let checkPublicValue visitorInfo (astNode:CurrentNode) (identifier:Ident) =
            if not <| isOperator identifier.idText then
                expectNoUnderscore visitorInfo astNode identifier

        let checkMember visitorInfo (astNode:CurrentNode) (identifier:Ident) =
            if not <| isOperator identifier.idText then
                if "MemberNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName, "MemberNamesMustBePascalCase") |> not then
                    expectPascalCase visitorInfo.PostError identifier
                
                expectNoUnderscore visitorInfo astNode identifier

        let checkNamespace visitorInfo (astNode:CurrentNode) identifier =
            if "NamespaceNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo astNode identifier

        let checkLiteral visitorInfo (astNode:CurrentNode) identifier =
            if "LiteralNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName, "LiteralNamesMustBePascalCase") |> not then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo astNode identifier

        let checkModule visitorInfo (astNode:CurrentNode) identifier =
            if "ModuleNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName, "ModuleNamesMustBePascalCase") |> not then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo astNode identifier

        let checkEnumCase visitorInfo (astNode:CurrentNode) identifier =
            if "EnumCasesMustBePascalCase" |> isRuleEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName, "EnumCasesMustBePascalCase") |> not then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo astNode identifier

        let checkUnionCase visitorInfo (astNode:CurrentNode) identifier =
            expectNoUnderscore visitorInfo astNode identifier

        let checkRecordField visitorInfo (astNode:CurrentNode) identifier =
            if "RecordFieldNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName, "RecordFieldNamesMustBePascalCase") |> not then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo astNode identifier

        let checkTypeName visitorInfo (astNode:CurrentNode) identifier =
            if TypeNamesMustBePascalCase |> isRuleEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName, TypeNamesMustBePascalCase) |> not then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo astNode identifier

        let checkParameter visitorInfo (astNode:CurrentNode) identifier =
            if "ParameterMustBeCamelCase" |> isRuleEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName, "ParameterMustBeCamelCase") |> not then
                expectCamelCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo astNode identifier

        let checkActivePattern visitorInfo (astNode:CurrentNode) (identifier:Ident) =
            if IdentifiersMustNotContainUnderscores |> isRuleEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName, IdentifiersMustNotContainUnderscores) |> not then
                let error ident =
                    if containsUnderscore ident then
                        let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesNamingConventionsUnderscoreError")
                        let error = System.String.Format(errorFormatString, ident)
                        visitorInfo.PostError identifier.idRange error

                identifier.idText.Split([|'|'|]).Where(fun x -> not <| String.IsNullOrEmpty(x) && x.Trim() <> "_")
                    |> Seq.iter error

        let checkException visitorInfo (astNode:CurrentNode) identifier =
            if TypeNamesMustBePascalCase |> isRuleEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName, TypeNamesMustBePascalCase) |> not then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo astNode identifier

            if "ExceptionNamesMustEndWithException" |> isRuleEnabled visitorInfo.Config && not <| identifier.idText.EndsWith("Exception") && astNode.IsSuppressed(AnalyserName, "ExceptionNamesMustEndWithException") |> not then
                let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesNamingConventionsExceptionError")
                let error = System.String.Format(errorFormatString, identifier.idText)
                visitorInfo.PostError identifier.idRange error

        let checkInterface visitorInfo (astNode:CurrentNode) identifier =
            if TypeNamesMustBePascalCase |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo astNode identifier

            if "InterfaceNamesMustBeginWithI" |> isRuleEnabled visitorInfo.Config && not <| identifier.idText.StartsWith("I") && astNode.IsSuppressed(AnalyserName, "InterfaceNamesMustBeginWithI") |> not then
                let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesNamingConventionsInterfaceError")
                let error = System.String.Format(errorFormatString, identifier.idText)
                visitorInfo.PostError identifier.idRange error
            
    let isActivePattern (identifier:Ident) =
        Microsoft.FSharp.Compiler.PrettyNaming.IsActivePatternName identifier.idText

    let isLiteral (attributes:SynAttributes) = 
        let isLiteralAttribute (attribute:SynAttribute) =
            let name = attribute.TypeName.Lid |> List.rev |> List.head

            name.idText = "LiteralAttribute" || name.idText = "Literal"
        attributes |> List.exists isLiteralAttribute

    /// Gets a visitor that checks all nodes on the AST where an identifier may be declared, 
    /// and post errors if any violate best practice guidelines.
    let rec visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, isModule, _, _, _, _, _)) -> 
                let checkIdent = 
                    if isModule then CheckIdentifiers.checkModule visitorInfo astNode
                    else CheckIdentifiers.checkNamespace visitorInfo astNode
                        
                identifier |> List.iter checkIdent
                Continue
            | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
                CheckIdentifiers.checkUnionCase visitorInfo astNode identifier
                Continue
            | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
                identifier |> Option.iter (CheckIdentifiers.checkRecordField visitorInfo astNode)
                Continue
            | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
                CheckIdentifiers.checkEnumCase visitorInfo astNode identifier
                Continue
            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(componentInfo, typeDef, _, _)) -> 
                let isTypeExtensions =
                    match typeDef with
                        | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconAugmentation, _, _) -> true
                        | _ -> false

                let isInterface() =
                    let hasConstructor = function 
                        | SynMemberDefn.ImplicitCtor(_) -> true 
                        | _ -> false

                    match typeDef with
                        | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconInterface, members, _)
                        | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconUnspecified, members, _) -> 
                            members |> List.exists hasConstructor |> not
                        | _ -> false
            
                if not isTypeExtensions then
                    match componentInfo with
                        | SynComponentInfo.ComponentInfo(_, _, _, identifier, _, _, _, _) -> 
                            let interfaceIdentifier = identifier.[List.length identifier - 1]
                
                            if isInterface() then 
                                CheckIdentifiers.checkInterface visitorInfo astNode interfaceIdentifier
                            else
                                identifier |> List.iter (CheckIdentifiers.checkTypeName visitorInfo astNode)
           
                Continue
            | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, _, _, _)) -> 
                match unionCase with
                    | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
                        CheckIdentifiers.checkException visitorInfo astNode identifier
                Continue
            | AstNode.Expression(SynExpr.For(_, identifier, _, _, _, _, _)) ->
                CheckIdentifiers.checkNonPublicValue visitorInfo astNode identifier
                Continue
            | AstNode.MemberDefinition(memberDef) ->
                match memberDef with
                    | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, identifier, _, _, _, _, _, _, _, _, _), _, _) ->
                        CheckIdentifiers.checkMember visitorInfo astNode identifier
                    | _ -> ()
                Continue
            | AstNode.Pattern(pattern) ->
                match pattern with
                    | SynPat.LongIdent(longIdentifier, identifier, _, _, _, _) -> 
                        //if isValue longIdentifier.Lid checkFile then
                        CheckIdentifiers.checkNonPublicValue visitorInfo astNode longIdentifier.Lid.Head
                    | SynPat.Named(_, identifier, isThis, _, _) when not isThis -> 
                        CheckIdentifiers.checkParameter visitorInfo astNode identifier
                    | _ -> ()
                Continue
            | AstNode.SimplePattern(pattern) ->
                match pattern with
                    | SynSimplePat.Id(identifier, _, isCompilerGenerated, _, _, _) when not isCompilerGenerated ->
                        CheckIdentifiers.checkParameter visitorInfo astNode identifier
                    | _ -> ()
                Continue
            | AstNode.Binding(SynBinding.Binding(_, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
                if isLiteral attributes then
                    match pattern with
                        | SynPat.Named(_, identifier, _, _, _) -> 
                            CheckIdentifiers.checkLiteral visitorInfo astNode identifier
                        | _ -> ()

                    Stop
                else
                    let getVisitorForChild i child =
                        match child with
                            | Pattern(_) ->
                                Some(bindingPatternVisitor visitorInfo checkFile valData)
                            | _ -> 
                                Some(visitor visitorInfo checkFile)

                    ContinueWithVisitorsForChildren(getVisitorForChild)
            | _ -> Continue
    and 
        bindingPatternVisitor visitorInfo checkFile valData astNode = 
            match astNode.Node with
                | AstNode.Pattern(pattern) ->
                    match pattern with
                        | SynPat.LongIdent(longIdentifier, identifier, _, _, _, _) -> 
                            let lastIdent = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

                            match identifierType longIdentifier.Lid checkFile valData with
                                | Value | Function when isActivePattern lastIdent ->
                                    CheckIdentifiers.checkActivePattern visitorInfo astNode lastIdent
                                    Continue
                                | Value | Function when (astNode.Node :: astNode.Breadcrumbs) |> isPublic -> 
                                    CheckIdentifiers.checkPublicValue visitorInfo astNode lastIdent
                                    ContinueWithVisitor(visitor visitorInfo checkFile)
                                | Value | Function ->
                                    CheckIdentifiers.checkNonPublicValue visitorInfo astNode lastIdent
                                    Continue
                                | Member | Property -> 
                                    CheckIdentifiers.checkMember visitorInfo astNode lastIdent
                                    Continue
                                | _ -> Continue
                        | SynPat.Named(_, identifier, isThis, _, _) when not isThis -> 
                            if isActivePattern identifier then
                                CheckIdentifiers.checkActivePattern visitorInfo astNode identifier
                            Continue
                        | _ -> Continue
                | _ -> Continue

    type RegisterNameConventionsVisitor() = 
        let plugin =
            {
                Name = AnalyserName
                Visitor = Ast(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin