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

module NestedStatements =
    
    open System
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.AstInfo
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "NestedStatements"
    (*
    let configDepth config =
        match isAnalyserEnabled config AnalyserName with
        | Some(analyserSettings) when analyserSettings.ContainsKey "Depth" ->
            match analyserSettings.["Depth"] with
            | Depth(l) -> Some(l)
            | _ -> None
        | Some(_) | None -> None

    let error (depth:int) =
        let errorFormatString = Resources.GetString("RulesNestedStatementsError")
        String.Format(errorFormatString, depth)

    exception UnexpectedNodeTypeException of string

    /// Lambda wildcard arguments are named internally as _argN, a match is then generated for them in the AST.
    /// e.g. fun _ -> () is represented in the AST as fun _arg1 -> match _arg1 with | _ -> ().
    /// This function returns true if the given match statement is compiler generated for a lmabda wildcard argument.
    let isCompilerGeneratedMatch = function
        | AstNode.Expression(SynExpr.Match(_, SynExpr.Ident(ident), _, _, _)) 
                when ident.idText.StartsWith("_arg") ->
            true
        | _ -> false

    let elseIfVisitor visitor depth visitorInfo checkFile =
        ContinueWithVisitorsForChildren (fun childi childNode ->
            match (childi, childNode) with
            | (2, AstNode.Expression(SynExpr.IfThenElse(_))) ->
                Some(visitor depth visitorInfo checkFile)
            | _ -> Some(visitor (depth + 1) visitorInfo checkFile))

    let visitor visitorInfo checkFile (syntaxArray:AbstractSyntaxArray.Node []) _ = 
        let mutable i = 0
        while i < syntaxArray.Length do
            match syntaxArray.[i].Actual with
            | AstNode.Binding(SynBinding.Binding(_))
            | AstNode.Expression(SynExpr.Lambda(_))
            | AstNode.Expression(SynExpr.MatchLambda(_))
            | AstNode.Expression(SynExpr.IfThenElse(_))
            | AstNode.Expression(SynExpr.Lazy(_))
            | AstNode.Expression(SynExpr.Match(_))
            | AstNode.Expression(SynExpr.Record(_))
            | AstNode.Expression(SynExpr.ObjExpr(_))
            | AstNode.Expression(SynExpr.TryFinally(_))
            | AstNode.Expression(SynExpr.TryWith(_))
            | AstNode.Expression(SynExpr.Tuple(_))
            | AstNode.Expression(SynExpr.Quote(_))
            | AstNode.Expression(SynExpr.While(_))
            | AstNode.Expression(SynExpr.For(_))
            | AstNode.Expression(SynExpr.ForEach(_)) as node 
                    when not (isLambdaALambdaArgument node || isCompilerGeneratedMatch node) -> 

                let range () =
                    match node with 
                    | AstNode.Expression(node) -> node.Range
                    | AstNode.Binding(node) -> node.RangeOfBindingAndRhs
                    | _ -> raise <| UnexpectedNodeTypeException("Expected an Expression or Binding node")

                match configDepth visitorInfo.Config with
                | Some(errorDepth) when depth >= errorDepth ->
                    visitorInfo.PostError (range()) (error errorDepth)
                | Some(_) ->
                    match node with
                    | AstNode.Expression(SynExpr.IfThenElse(_)) ->
                        elseIfVisitor visitor depth visitorInfo checkFile
                    | _ -> ContinueWithVisitor(visitor (depth + 1) visitorInfo checkFile)
                | None -> ()
            | _ -> ()

            i <- i + 1*)
    ()