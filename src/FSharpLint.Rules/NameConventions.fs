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
    open FSharpLint.Framework.TypeChecking

    let operatorIdentifiers = [
        "op_Nil"
        "op_ColonColon"
        "op_Addition"
        "op_Splice"
        "op_SpliceUntyped"
        "op_Increment"
        "op_Decrement"
        "op_Subtraction"
        "op_Multiply"
        "op_Exponentiation"
        "op_Division"
        "op_Append"
        "op_Concatenate"
        "op_Modulus"
        "op_BitwiseAnd"
        "op_BitwiseOr"
        "op_ExclusiveOr"
        "op_LeftShift"
        "op_LogicalNot"
        "op_RightShift"
        "op_UnaryPlus"
        "op_UnaryNegation"
        "op_AddressOf"
        "op_IntegerAddressOf"
        "op_BooleanAnd"
        "op_BooleanOr"
        "op_LessThanOrEqual"
        "op_Equality"
        "op_Inequality"
        "op_GreaterThanOrEqual"
        "op_LessThan"
        "op_GreaterThan"
        "op_PipeRight"
        "op_PipeRight2"
        "op_PipeRight3"
        "op_PipeLeft"
        "op_PipeLeft2"
        "op_PipeLeft3"
        "op_Dereference"
        "op_ComposeRight"
        "op_ComposeLeft"
        "op_TypedQuotationUnicode"
        "op_ChevronsBar"
        "op_Quotation"
        "op_QuotationUntyped"
        "op_AdditionAssignment"
        "op_SubtractionAssignment"
        "op_MultiplyAssignment"
        "op_DivisionAssignment"
        "op_Range"
        "op_RangeStep"
        "op_Dynamic"
        "op_DynamicAssignment"
        "op_ArrayLookup"
        "op_ArrayAssign"
    ]    

    /// Operator identifiers can be made up of "op_" followed by a sequence of operators from this list.
    let operators = [ 
        "Greater"
        "Less" 
        "Plus"
        "Minus"
        "Multiply"
        "Equals"
        "Twiddle"
        "Percent"
        "Dot"
        "Dollar"
        "Amp"
        "Bar"
        "At"
        "Hash"
        "Hat"
        "Bang"
        "Qmark"
        "Divide"
        "Colon"
        "LParen"
        "Comma"
        "RParen"
        "Space"
        "LBrack"
        "RBrack" 
    ]

    let rec isSequenceOfOperators (str:string) =
        if Seq.isEmpty str then
            true
        else
            let operator = operators |> List.tryFind (fun op -> str.StartsWith(op))

            match operator with
            | Some(operator) -> str.Substring(operator.Length) |> isSequenceOfOperators
            | None -> false

    /// Is an identifier an operator overload?
    let isOperator (identifier:string) =
        if operatorIdentifiers |> List.exists (fun x -> x = identifier) then
            true
        else
            if identifier.StartsWith("op_") && identifier.Length > 3 then
                let identifier = identifier.Substring(3)

                isSequenceOfOperators identifier
            else
                false

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

    let isSymbolAnInterface = function
        | Some(symbol:FSharpSymbol) ->
            match symbol with
            | :? FSharpEntity as entity when entity.IsInterface -> true
            | _ -> false
        | None -> false

    /// Validate the name conventions of an active pattern definition (for example '|Dog|Cat|').
    let expectValidActivePatternDefinition postError (identifier:Ident) =
        let error ident =
            if containsUnderscore ident then
                let error = sprintf "Identifiers should not contain underscores, but one was found in: %s" ident
                postError identifier.idRange error

        identifier.idText.Split([|'|'|]).Where(fun x -> not <| String.IsNullOrEmpty(x) && x.Trim() <> "_")
            |> Seq.iter error

    let checkComponentInfo postError (checkFile:CheckFileResults) (identifier:LongIdent) =
        let interfaceIdentifier = identifier.[List.length identifier - 1]
        let interfaceRange = interfaceIdentifier.idRange

        let startLine, endColumn = interfaceRange.StartLine, interfaceRange.EndColumn

        let symbol = checkFile.GetSymbolAtLocation(startLine - 1, endColumn, "", [interfaceIdentifier.idText])

        /// Is symbol the same identifier but declared elsewhere?
        let isSymbolAnExtensionType = function
            | Some(symbol:FSharpSymbol) ->
                match symbol with
                    | :? FSharpEntity as entity -> 
                        entity.DeclarationLocation.Start <> interfaceRange.Start
                        && entity.DisplayName = (interfaceIdentifier).idText
                    | _ -> false
            | None -> false
                
        if not <| isSymbolAnExtensionType symbol then
            identifier |> List.iter (expectPascalCase postError)

        if isSymbolAnInterface symbol then 
            if not <| interfaceIdentifier.idText.StartsWith("I") then
                let error = "Interface identifiers should begin with the letter I found interface " + interfaceIdentifier.idText
                postError interfaceRange error

    let checkException postError (checkFile:CheckFileResults) = function
        | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
            if not <| identifier.idText.EndsWith("Exception") then
                let error = sprintf "Exception identifier should end with 'Exception', but was %s" identifier.idText
                postError identifier.idRange error

    let isNamedPatternActivePattern (checkFile:CheckFileResults) (identifier:Ident) =
        let line, endColumn, ident = identifier.idRange.EndLine, identifier.idRange.EndColumn, identifier.idText

        let symbol = checkFile.GetSymbolAtLocation(line - 1, endColumn, "", [ident])

        let isActivePattern = function
            | Some(symbol:FSharpSymbol) ->
                match symbol with
                    | :? FSharpMemberFunctionOrValue as memberFunctionOrValue when memberFunctionOrValue.IsActivePattern -> 
                        true
                    | _ -> false
            | None -> false

        symbol |> isActivePattern

    let checkNamedPattern postError (checkFile:CheckFileResults) (identifier:Ident) =
        if isNamedPatternActivePattern checkFile identifier then
            expectValidActivePatternDefinition postError identifier
        else
            expectCamelCase postError identifier

    let checkLongIdentPattern postError (checkFile:CheckFileResults) (longIdentifier:LongIdentWithDots) (identifier:Ident option) =
        let lastIdent = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

        match longIdentPatternType longIdentifier identifier checkFile with
            | Member when lastIdent.idText <> "new" -> expectPascalCase postError lastIdent
            | ValueOrFunction -> expectCamelCase postError lastIdent
            | ActivePatternDefinition -> expectValidActivePatternDefinition postError lastIdent
            | _ -> ()

    let isLiteral (attributes:SynAttributes) (checkFile:CheckFileResults) = 
        let isLiteralAttribute (attribute:SynAttribute) =
            let range = attribute.TypeName.Range
            let names = attribute.TypeName.Lid |> List.map (fun x -> x.idText)
            let symbol = checkFile.GetSymbolAtLocation(range.EndLine, range.EndColumn, "", names)
            match symbol with
                | Some(symbol) -> 
                    match symbol with
                        | :? FSharpEntity as entity when entity.DisplayName = "LiteralAttribute" -> 
                            match entity.Namespace with
                                | Some(name) when name.EndsWith("FSharp.Core") -> true
                                | _ -> false
                        | _ -> false
                | _ -> false

        attributes |> List.exists isLiteralAttribute

    /// Gets a visitor that checks all nodes on the AST where an identifier may be declared, 
    /// and post errors if any violate best practice guidelines.
    let rec visitor visitorInfo checkFile astNode = 
        let expectCamelCase, expectPascalCase = expectCamelCase visitorInfo.PostError, expectPascalCase visitorInfo.PostError

        match astNode.Node with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, _, _, _, _, _, _)) -> 
                identifier |> List.iter expectPascalCase
                Continue
            | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
                expectPascalCase identifier
                Continue
            | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
                identifier |> Option.iter expectPascalCase
                Continue
            | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
                expectPascalCase identifier
                Continue
            | AstNode.ComponentInfo(SynComponentInfo.ComponentInfo(_, _, _, identifier, _, _, _, _)) ->
                checkComponentInfo visitorInfo.PostError checkFile identifier
                Continue
            | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, _, _, _)) -> 
                checkException visitorInfo.PostError checkFile unionCase
                Continue
            | AstNode.Expression(expr) ->
                match expr with
                    | SynExpr.For(_, identifier, _, _, _, _, _) ->
                        expectCamelCase identifier
                    | _ -> ()
                Continue
            | AstNode.MemberDefinition(memberDef) ->
                match memberDef with
                    | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, identifier, _, _, _, _, _, _, _, _, _), _, _) ->
                        expectPascalCase identifier
                    | _ -> ()
                Continue
            | AstNode.Pattern(pattern) ->
                match pattern with
                    | SynPat.LongIdent(longIdentifier, identifier, _, _, _, _) -> 
                        checkLongIdentPattern visitorInfo.PostError checkFile longIdentifier identifier
                    | SynPat.Named(_, identifier, _, _, _) -> 
                        checkNamedPattern visitorInfo.PostError checkFile identifier
                    | _ -> ()
                Continue
            | AstNode.SimplePattern(pattern) ->
                match pattern with
                    | SynSimplePat.Id(identifier, _, isCompilerGenerated, _, _, _) ->
                        if not <| isCompilerGenerated then
                            expectCamelCase identifier
                    | _ -> ()
                Continue
            | AstNode.Binding(binding) ->
                match binding with
                    | SynBinding.Binding(_, _, _, _, attributes, _, _, pattern, _, _, _, _) -> 
                        if isLiteral attributes checkFile then
                            match pattern with
                                | SynPat.Named(_, identifier, _, _, _) -> 
                                    expectPascalCase identifier
                                | _ -> ()

                            Stop
                        else
                            let getVisitorForChild i child =
                                match child with
                                    | Pattern(_) ->
                                        Some(bindingPatternVisitor visitorInfo checkFile)
                                    | _ -> 
                                        Some(visitor visitorInfo checkFile)

                            ContinueWithVisitorsForChildren(getVisitorForChild)
            | _ -> Continue
    and 
        bindingPatternVisitor visitorInfo checkFile astNode = 
            match astNode.Node with
                | AstNode.Pattern(pattern) ->
                    match pattern with
                        | SynPat.LongIdent(longIdentifier, identifier, _, _, _, _) -> 
                            let lastIdent = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

                            match longIdentPatternType longIdentifier identifier checkFile with
                                | Member when lastIdent.idText <> "new" -> 
                                    expectPascalCase visitorInfo.PostError lastIdent
                                    Continue
                                | ValueOrFunction when (astNode.Node :: astNode.Breadcrumbs) |> isPublic -> 
                                    expectNoUnderscore visitorInfo.PostError lastIdent
                                    ContinueWithVisitor(visitor visitorInfo checkFile)
                                | ValueOrFunction -> 
                                    expectCamelCase visitorInfo.PostError lastIdent
                                    Continue
                                | ActivePatternDefinition -> 
                                    expectValidActivePatternDefinition visitorInfo.PostError lastIdent
                                    Continue
                                | _ -> Continue
                        | SynPat.Named(_, identifier, _, _, _) -> 
                            if isNamedPatternActivePattern checkFile identifier then
                                expectValidActivePatternDefinition visitorInfo.PostError identifier
                            else
                                expectNoUnderscore visitorInfo.PostError identifier
                            Continue
                        | _ -> Continue
                | AstNode.SimplePattern(pattern) ->
                    match pattern with
                        | SynSimplePat.Id(identifier, _, isCompilerGenerated, _, _, _) ->
                            expectNoUnderscore visitorInfo.PostError identifier
                        | _ -> ()
                    Continue
                | _ -> Continue