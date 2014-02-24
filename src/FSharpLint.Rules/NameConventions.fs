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
    open FSharpLint.Framework.AstVisitorBase

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
    let isOperator (identifier:Ident) =
        if operatorIdentifiers |> List.exists (fun x -> x = identifier.idText) then
            true
        else
            if identifier.idText.StartsWith("op_") && identifier.idText.Length > 3 then
                let identifier = identifier.idText.Substring(3)

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
        if not <| isOperator identifier then
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

    /// Whether or not an identifier is representing a class property.
    type IsProperty =
    | Property of range
    | NotProperty

    /// Is an identifier a class property?
    let isIdentifierAProperty = function
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

    /// Possible types a long identifier as part of a pattern may be representing.
    type LongIdentPatternType =
    | UnionCase of FSharpUnionCase
    | Member of FSharpMemberFunctionOrValue
    | ActivePatternDefinition of FSharpMemberFunctionOrValue
    | ActivePatternCase of FSharpSymbol
    | ValueOrFunction of FSharpMemberFunctionOrValue
    | Other

    let longIdentPatternType (longIdentifier:LongIdentWithDots) (identifier:Ident option) (checkFile:CheckFileResults) = 
        let name = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

        let range = match isIdentifierAProperty identifier with
                    | Property(range) -> range
                    | NotProperty -> name.idRange

        let line, endColumn, ident = range.EndLine, range.EndColumn, name.idText

        let symbol = checkFile.GetSymbolAtLocation(line - 1, endColumn, "", [ident])
        
        match symbol with
        | Some(symbol) ->
            match symbol with
            | :? FSharpUnionCase as unionCase -> UnionCase unionCase
            | :? FSharpMemberFunctionOrValue as memberFunctionOrValue -> 
                if memberFunctionOrValue.IsMember then
                    Member memberFunctionOrValue
                else if memberFunctionOrValue.IsActivePattern then
                    ActivePatternDefinition memberFunctionOrValue
                else 
                    ValueOrFunction memberFunctionOrValue
            | _ -> Other
        | None -> 
            Other

    /// Gets a visitor that checks all nodes on the AST where an identifier may be declared, 
    /// and post errors if any violate best practice guidelines.
    let visitor postError checkFile = 
        let expectCamelCase, expectPascalCase = expectCamelCase postError, expectPascalCase postError

        { new AstVisitorBase(checkFile) with
            member this.VisitModuleOrNamespace(identifier, _, _, _, _, _, range) = 
                identifier |> List.iter expectPascalCase
                [this]

            member this.VisitUnionCase(_, identifier, _, _, _, range) = 
                expectPascalCase identifier
                [this]

            member this.VisitNamedPattern(_, identifier, _, _, range) = 
                let line, endColumn, ident = identifier.idRange.EndLine, identifier.idRange.EndColumn, identifier.idText

                let symbol = checkFile.GetSymbolAtLocation(line - 1, endColumn, "", [ident])
        
                match symbol with
                | Some(symbol) ->
                    match symbol with
                    | :? FSharpMemberFunctionOrValue as memberFunctionOrValue when memberFunctionOrValue.IsActivePattern -> 
                        expectValidActivePatternDefinition postError identifier
                    | _ -> expectCamelCase identifier
                | None -> 
                    expectCamelCase identifier

                [this]

            member this.VisitIdPattern(identifier, isCompilerGenerated, range) = 
                if not <| isCompilerGenerated then
                    expectCamelCase identifier
                [this]

            member this.VisitField(_, identifier, _, _, _, range) = 
                identifier |> Option.iter expectPascalCase
                [this]

            member this.VisitEnumCase(_, identifier, _, _, range) = 
                expectPascalCase identifier
                [this]

            member this.VisitComponentInfo(_, _, _, identifier, _, _, range) = 
                identifier |> List.iter expectPascalCase

                let interfaceIdentifier = identifier.Head
                let interfaceRange = interfaceIdentifier.idRange

                let startLine, endColumn = interfaceRange.StartLine, interfaceRange.EndColumn

                if isSymbolAnInterface <| checkFile.GetSymbolAtLocation(startLine - 1, endColumn, "", [interfaceIdentifier.idText]) then 
                    if not <| interfaceIdentifier.idText.StartsWith("I") then
                        let error = "Interface identifiers should begin with the letter I found interface " + interfaceIdentifier.idText
                        postError interfaceRange error
                    
                [this]

            member this.VisitLongIdentPattern(longIdentifier, identifier, _, access, range) =  
                let lastIdent = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

                match longIdentPatternType longIdentifier identifier checkFile with
                | Member(_) when lastIdent.idText <> "new" -> expectPascalCase lastIdent
                | ValueOrFunction(_) -> expectCamelCase lastIdent
                | ActivePatternDefinition(_) -> expectValidActivePatternDefinition postError lastIdent
                | _ -> ()

                [this]

            member this.VisitExceptionRepresentation(_, unionCase, _, _, _, _) = 
                match unionCase with 
                | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
                    if not <| identifier.idText.EndsWith("Exception") then
                        let error = sprintf "Exception identifier should end with 'Exception', but was %s" identifier.idText
                        postError identifier.idRange error
                
                [this]

            /// If inside of an implementation file the node must be an abstract member signature.
            member this.VisitValueSignature(identifier, range) = 
                expectPascalCase identifier
                [this]

            member this.VisitFor(identifier, range) = 
                expectCamelCase identifier
                [this]
        }