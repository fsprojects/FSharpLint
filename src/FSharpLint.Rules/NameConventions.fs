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
    open FSharpLint.Framework.LoadAnalysers

    [<Literal>]
    let AnalyserName = "FSharpLint.NameConventions"

    let isRuleEnabled config ruleName =
        match isRuleEnabled config AnalyserName ruleName with
            | Some(_) -> true
            | None -> false

    let isPascalCase (identifier:string) = Regex.Match(identifier, @"^[A-Z]([a-z]|[A-Z]|\d)*").Success

    let isCamelCase (identifier:string) = Regex.Match(identifier, @"^[a-z]([a-z]|[A-Z]|\d)*").Success

    let containsUnderscore (identifier:string) = identifier.Contains("_")

    let pascalCaseError (identifier:string) = 
        sprintf "Expected pascal case identifier but was %s" identifier

    let camelCaseError (identifier:string) = 
        sprintf "Expected camel case identifier but was %s" identifier

    let expectNoUnderscore postError (identifier:Ident) =
        if containsUnderscore identifier.idText then
            let error = sprintf "Identifiers should not contain underscores, but one was found in %s" identifier.idText
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

        let private expectNoUnderscore visitorInfo identifier = 
            if IdentifiersMustNotContainUnderscores |> isRuleEnabled visitorInfo.Config then
                expectNoUnderscore visitorInfo.PostError identifier

        let checkNonPublicValue visitorInfo (identifier:Ident) =
            if not <| isOperator identifier.idText then
                if "NonPublicValuesCamelCase" |> isRuleEnabled visitorInfo.Config then
                    expectCamelCase visitorInfo.PostError identifier

                expectNoUnderscore visitorInfo identifier

        let checkPublicValue visitorInfo (identifier:Ident) =
            if not <| isOperator identifier.idText then
                expectNoUnderscore visitorInfo identifier

        let checkMember visitorInfo (identifier:Ident) =
            if not <| isOperator identifier.idText then
                if "MemberNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config then
                    expectPascalCase visitorInfo.PostError identifier
                
                expectNoUnderscore visitorInfo identifier

        let checkNamespace visitorInfo identifier =
            if "NamespaceNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkLiteral visitorInfo identifier =
            if "LiteralNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkModule visitorInfo identifier =
            if "ModuleNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkEnumCase visitorInfo identifier =
            if "EnumCasesMustBePascalCase" |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkUnionCase visitorInfo identifier =
            expectNoUnderscore visitorInfo identifier

        let checkRecordField visitorInfo identifier =
            if "RecordFieldNamesMustBePascalCase" |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkTypeName visitorInfo identifier =
            if TypeNamesMustBePascalCase |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkParameter visitorInfo identifier =
            if "ParameterMustBeCamelCase" |> isRuleEnabled visitorInfo.Config then
                expectCamelCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

        let checkActivePattern visitorInfo (identifier:Ident) =
            if IdentifiersMustNotContainUnderscores |> isRuleEnabled visitorInfo.Config then
                let error ident =
                    if containsUnderscore ident then
                        let error = sprintf "Identifiers should not contain underscores, but one was found in: %s" ident
                        visitorInfo.PostError identifier.idRange error

                identifier.idText.Split([|'|'|]).Where(fun x -> not <| String.IsNullOrEmpty(x) && x.Trim() <> "_")
                    |> Seq.iter error

        let checkException visitorInfo identifier =
            if TypeNamesMustBePascalCase |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

            if "ExceptionNamesMustEndWithException" |> isRuleEnabled visitorInfo.Config && not <| identifier.idText.EndsWith("Exception") then
                let error = sprintf "Exception identifier should end with 'Exception', but was %s" identifier.idText
                visitorInfo.PostError identifier.idRange error

        let checkInterface visitorInfo identifier =
            if TypeNamesMustBePascalCase |> isRuleEnabled visitorInfo.Config then
                expectPascalCase visitorInfo.PostError identifier
                
            expectNoUnderscore visitorInfo identifier

            if "InterfaceNamesMustBeginWithI" |> isRuleEnabled visitorInfo.Config && not <| identifier.idText.StartsWith("I") then
                let error = "Interface identifiers should begin with the letter I found interface " + identifier.idText
                visitorInfo.PostError identifier.idRange error

    let isSymbolAnInterface = function
        | Some(symbol:FSharpSymbolUse) ->
            match symbol.Symbol with
            | :? FSharpEntity as entity when entity.IsInterface -> true
            | _ -> false
        | None -> false

    let checkComponentInfo visitorInfo (checkFile:CheckFileResults) (identifier:LongIdent) =
        let interfaceIdentifier = identifier.[List.length identifier - 1]
        let interfaceRange = interfaceIdentifier.idRange

        let startLine, endColumn = interfaceRange.StartLine, interfaceRange.EndColumn

        let symbol = checkFile.GetSymbolUseAtLocation(startLine, endColumn, "", [interfaceIdentifier.idText])
                                |> Async.RunSynchronously

        /// Is symbol the same identifier but declared elsewhere?
        let isSymbolAnExtensionType = function
            | Some(symbol:FSharpSymbolUse) ->
                match symbol.Symbol with
                    | :? FSharpEntity as entity -> 
                        entity.DeclarationLocation.Start <> interfaceRange.Start
                        && entity.DisplayName = (interfaceIdentifier).idText
                    | _ -> false
            | None -> false
                
        if isSymbolAnInterface symbol then 
            CheckIdentifiers.checkInterface visitorInfo interfaceIdentifier
        else if not <| isSymbolAnExtensionType symbol then
            identifier |> List.iter (CheckIdentifiers.checkTypeName visitorInfo)
            
    let isActivePattern (identifier:Ident) =
        Microsoft.FSharp.Compiler.PrettyNaming.IsActivePatternName identifier.idText

    let isLiteral (attributes:SynAttributes) (checkFile:CheckFileResults) = 
        let isLiteralAttribute (attribute:SynAttribute) =
            let range = attribute.TypeName.Range
            let names = attribute.TypeName.Lid |> List.map (fun x -> x.idText)
            let symbol = checkFile.GetSymbolUseAtLocation(range.EndLine + 1, range.EndColumn, "", names)
                            |> Async.RunSynchronously
            match symbol with
                | Some(symbol) -> 
                    match symbol.Symbol with
                        | :? FSharpEntity as entity when 
                                entity.IsFSharpAbbreviation &&
                                entity.AbbreviatedType.TypeDefinition.DisplayName = "LiteralAttribute" -> 
                            match entity.AbbreviatedType.TypeDefinition.Namespace with
                                | Some(name) when name.EndsWith("FSharp.Core") -> true
                                | _ -> false
                        | :? FSharpEntity as entity when 
                                entity.IsClass && entity.DisplayName = "LiteralAttribute" -> 
                            match entity.Namespace with
                                | Some(name) when name.EndsWith("FSharp.Core") -> true
                                | _ -> false
                        | _ -> false
                | _ -> false

        attributes |> List.exists isLiteralAttribute

    /// Gets a visitor that checks all nodes on the AST where an identifier may be declared, 
    /// and post errors if any violate best practice guidelines.
    let rec visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, isModule, _, _, _, _, _)) -> 
                let checkIdent = 
                    if isModule then CheckIdentifiers.checkModule visitorInfo
                    else CheckIdentifiers.checkNamespace visitorInfo
                        
                identifier |> List.iter checkIdent
                Continue
            | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
                CheckIdentifiers.checkUnionCase visitorInfo identifier
                Continue
            | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
                identifier |> Option.iter (CheckIdentifiers.checkRecordField visitorInfo)
                Continue
            | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
                CheckIdentifiers.checkEnumCase visitorInfo identifier
                Continue
            | AstNode.ComponentInfo(SynComponentInfo.ComponentInfo(_, _, _, identifier, _, _, _, _)) ->
                checkComponentInfo visitorInfo checkFile identifier
                Continue
            | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, _, _, _)) -> 
                match unionCase with
                    | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
                        CheckIdentifiers.checkException visitorInfo identifier
                Continue
            | AstNode.Expression(expr) ->
                match expr with
                    | SynExpr.For(_, identifier, _, _, _, _, _) ->
                        CheckIdentifiers.checkNonPublicValue visitorInfo identifier
                    | _ -> ()
                Continue
            | AstNode.MemberDefinition(memberDef) ->
                match memberDef with
                    | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, identifier, _, _, _, _, _, _, _, _, _), _, _) ->
                        CheckIdentifiers.checkMember visitorInfo identifier
                    | _ -> ()
                Continue
            | AstNode.Pattern(pattern) ->
                match pattern with
                    | SynPat.LongIdent(longIdentifier, identifier, _, _, _, _) -> 
                        if isValue longIdentifier.Lid checkFile then
                            CheckIdentifiers.checkNonPublicValue visitorInfo longIdentifier.Lid.Head
                    | SynPat.Named(_, identifier, isThis, _, _) when not isThis -> 
                        CheckIdentifiers.checkParameter visitorInfo identifier
                    | _ -> ()
                Continue
            | AstNode.SimplePattern(pattern) ->
                match pattern with
                    | SynSimplePat.Id(identifier, _, isCompilerGenerated, _, _, _) when not isCompilerGenerated ->
                        CheckIdentifiers.checkParameter visitorInfo identifier
                    | _ -> ()
                Continue
            | AstNode.Binding(binding) ->
                match binding with
                    | SynBinding.Binding(_, _, _, _, attributes, _, valData, pattern, _, _, _, _) -> 
                        if isLiteral attributes checkFile then
                            match pattern with
                                | SynPat.Named(_, identifier, _, _, _) -> 
                                    CheckIdentifiers.checkLiteral visitorInfo identifier
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
                                    CheckIdentifiers.checkActivePattern visitorInfo lastIdent
                                    Continue
                                | Value | Function when (astNode.Node :: astNode.Breadcrumbs) |> isPublic -> 
                                    CheckIdentifiers.checkPublicValue visitorInfo lastIdent
                                    ContinueWithVisitor(visitor visitorInfo checkFile)
                                | Value | Function ->
                                    CheckIdentifiers.checkNonPublicValue visitorInfo lastIdent
                                    Continue
                                | Member | Property -> 
                                    CheckIdentifiers.checkMember visitorInfo lastIdent
                                    Continue
                                | _ -> Continue
                        | SynPat.Named(_, identifier, isThis, _, _) when not isThis -> 
                            if isActivePattern identifier then
                                CheckIdentifiers.checkActivePattern visitorInfo identifier
                            Continue
                        | _ -> Continue
                | _ -> Continue

    type RegisterXmlDocumentationAnalyser() = 
        let plugin =
            {
                Name = AnalyserName
                Analyser = Ast(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin