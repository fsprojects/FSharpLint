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

    /// Is an identifier a class property?
    let (|Property|NotProperty|) = function
        | Some(identifier:Ident) ->
            match identifier.idText with
            | "set" | "get" -> Property identifier.idRange
            | _ ->  NotProperty
        | _ ->  NotProperty

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

    let checkNamedPattern postError (checkFile:CheckFileResults) (identifier:Ident) =
        let line, endColumn, ident = identifier.idRange.EndLine, identifier.idRange.EndColumn, identifier.idText

        let symbol = checkFile.GetSymbolAtLocation(line - 1, endColumn, "", [ident])
        
        match symbol with
            | Some(symbol) ->
                match symbol with
                    | :? FSharpMemberFunctionOrValue as memberFunctionOrValue when memberFunctionOrValue.IsActivePattern -> 
                        expectValidActivePatternDefinition postError identifier
                    | _ -> expectCamelCase postError identifier
            | None -> 
                expectCamelCase postError identifier

    /// Possible types a long identifier as part of a pattern may be representing.
    type LongIdentPatternType =
        | Member
        | ActivePatternDefinition
        | ValueOrFunction
        | Other

    let longIdentPatternType (longIdentifier:LongIdentWithDots) (identifier:Ident option) (checkFile:CheckFileResults) = 
        let name = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

        let range = match identifier with
                    | Property(range) -> range
                    | NotProperty -> name.idRange

        let line, endColumn, ident = range.EndLine, range.EndColumn, name.idText

        let symbol = checkFile.GetSymbolAtLocation(line - 1, endColumn, "", [ident])
        
        match symbol with
            | Some(symbol) ->
                match symbol with
                    | :? FSharpMemberFunctionOrValue as memberFunctionOrValue -> 
                        if memberFunctionOrValue.IsMember then
                            Member
                        else if memberFunctionOrValue.IsActivePattern then
                            ActivePatternDefinition
                        else 
                            ValueOrFunction
                    | _ -> Other
            | None -> 
                Other

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
    let visitor postError (checkFile:CheckFileResults) path astNode = 
        let expectCamelCase, expectPascalCase = expectCamelCase postError, expectPascalCase postError

        match astNode with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, _, _, _, _, _, _)) -> 
                identifier |> List.iter expectPascalCase
                ContinueWalk
            | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
                expectPascalCase identifier
                ContinueWalk
            | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
                identifier |> Option.iter expectPascalCase
                ContinueWalk
            | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
                expectPascalCase identifier
                ContinueWalk
            | AstNode.ComponentInfo(SynComponentInfo.ComponentInfo(_, _, _, identifier, _, _, _, _)) ->
                checkComponentInfo postError checkFile identifier
                ContinueWalk
            | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, _, _, _)) -> 
                checkException postError checkFile unionCase
                ContinueWalk
            | AstNode.Expression(expr) ->
                match expr with
                    | SynExpr.For(_, identifier, _, _, _, _, _) ->
                        expectCamelCase identifier
                    | _ -> ()
                ContinueWalk
            | AstNode.MemberDefinition(memberDef) ->
                match memberDef with
                    | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, identifier, _, _, _, _, _, _, _, _, _), _, _) ->
                        expectPascalCase identifier
                    | _ -> ()
                ContinueWalk
            | AstNode.Pattern(pattern) ->
                match pattern with
                    | SynPat.LongIdent(longIdentifier, identifier, _, _, _, _) -> 
                        checkLongIdentPattern postError checkFile longIdentifier identifier
                    | SynPat.Named(_, identifier, _, _, _) -> 
                        checkNamedPattern postError checkFile identifier
                    | _ -> ()
                ContinueWalk
            | AstNode.SimplePattern(pattern) ->
                match pattern with
                    | SynSimplePat.Id(identifier, _, isCompilerGenerated, _, _, _) ->
                        if not <| isCompilerGenerated then
                            expectCamelCase identifier
                    | _ -> ()
                ContinueWalk
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
                            ContinueWalk
            | _ -> ContinueWalk