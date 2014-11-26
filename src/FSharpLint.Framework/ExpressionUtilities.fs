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

module ExpressionUtilities =

    open Microsoft.FSharp.Compiler.Ast

    /// Converts an operator name e.g. op_Add to the operator symbol e.g. +
    let identAsDecompiledOpName (ident:Ident) =
        if ident.idText.StartsWith("op_") then
            Microsoft.FSharp.Compiler.PrettyNaming.DecompileOpName ident.idText
        else 
            ident.idText

    /// Extracts an expression from parentheses e.g. ((x + 4)) -> x + 4
    let rec removeParens = function
        | SynExpr.Paren(x, _, _, _) -> removeParens x
        | x -> x
        
    let flattenFunctionApplication expr =
        let listContains value list = List.exists ((=) value) list

        let isForwardPipeOperator op = ["|>";"||>";"|||>"] |> listContains op
        let isBackwardPipeOperator op = ["<|";"<||";"<|||"] |> listContains op

        let rec flattenFunctionApplication exprs = function
            | SynExpr.App(_, _, x, y, _) -> 
                match removeParens x with
                    | SynExpr.App(_, true, SynExpr.Ident(op), rightExpr, _) ->
                        let opIdent = identAsDecompiledOpName op

                        if isForwardPipeOperator opIdent then
                            let flattened = flattenFunctionApplication [] y
                            flattened@[rightExpr]
                        else if isBackwardPipeOperator opIdent then
                            let flattened = flattenFunctionApplication [] rightExpr
                            flattened@[y]
                        else
                            flattenFunctionApplication (y::exprs) x
                    | _ -> 
                        flattenFunctionApplication (y::exprs) x
            | x -> 
                x::exprs

        flattenFunctionApplication [] expr