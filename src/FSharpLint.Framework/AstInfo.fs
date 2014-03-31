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

namespace FSharpLint.Framework

module AstInfo =

    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices

    let isValue (identifier:LongIdent) (checkFile:CheckFileResults) =
        if System.Char.IsUpper(identifier.Head.idText.[0]) && identifier.Length = 1 then
            let identifier = identifier.Head

            let startLine, endColumn = identifier.idRange.StartLine, identifier.idRange.EndColumn

            let symbol = checkFile.GetSymbolUseAtLocation(startLine, endColumn, "", [identifier.idText])
                            |> Async.RunSynchronously

            match symbol with
                | Some(symbol) ->
                    match symbol.Symbol with
                        | :? FSharpMemberFunctionOrValue -> true
                        | _ -> false
                | None -> false
        else 
            identifier.Length = 1

    type IdentifierType =
        | Member
        | Function
        | Value
        | Constructor
        | Property
        | Other

    let identifierTypeFromValData = function
        | SynValData.SynValData(memberFlags, valInfo, _) -> 
            match memberFlags with
                | Some(memberFlags) -> 
                    match memberFlags.MemberKind with
                        | MemberKind.Constructor(_)
                        | MemberKind.ClassConstructor(_) -> Constructor
                        | MemberKind.Member(_) -> Member
                        | MemberKind.PropertyGet(_)
                        | MemberKind.PropertySet(_)
                        | MemberKind.PropertyGetSet(_) -> Property
                | None when valInfo.ArgInfos.Length = 0 -> Value
                | None -> Function
    
    let identifierType (identifier:LongIdent) (checkFile:CheckFileResults) valData =
        match identifierTypeFromValData valData with
            | Value -> 
                if isValue identifier checkFile then
                    Value
                else
                    Other
            | identifierType -> identifierType

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