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
    
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors

    [<Literal>]
    let AnalyserName = "NestedStatements"

    let configDepth config =
        match isAnalyserEnabled config AnalyserName with
            | Some(analyserSettings) when analyserSettings.ContainsKey "Depth" ->
                match analyserSettings.["Depth"] with
                    | Depth(l) -> Some(l)
                    | _ -> None
            | Some(_)
            | None -> None

    let error (depth:int) =
        let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesNestedStatementsError")
        System.String.Format(errorFormatString, depth)

    exception UnexpectedNodeTypeException of string

    /// Lambda arguments (after the first argument) are curried and represented as such internally.
    /// e.g. fun x y -> () will be represented in the AST as fun x -> fun y -> ().
    /// This function returns true if the given lambda is an argument.
    let isLambdaALambdaArgument = function
        | AstNode.Expression(SynExpr.Lambda(_, _, _, SynExpr.Lambda(_, _, _, _, nestedRange), range)) -> 
            range.StartLine = nestedRange.StartLine && range.StartColumn = nestedRange.StartColumn
        | _ -> false

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
                | _ -> 
                    Some(visitor (depth + 1) visitorInfo checkFile))
    
    let rec visitor depth (visitorInfo:VisitorInfo) checkFile astNode = 
        match astNode.Node with
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
                    when astNode.IsSuppressed(AnalyserName) |> not && 
                         not (isLambdaALambdaArgument node || isCompilerGeneratedMatch node) -> 

                let range () =
                    match node with 
                        | AstNode.Expression(node) ->
                            node.Range
                        | AstNode.Binding(node) -> 
                            node.RangeOfBindingAndRhs
                        | _ -> 
                            raise <| UnexpectedNodeTypeException("Expected an Expression or Binding node")

                match configDepth visitorInfo.Config with
                    | Some(errorDepth) when depth >= errorDepth ->
                        visitorInfo.PostError (range()) (error errorDepth)
                        Stop
                    | Some(_) ->
                        match astNode.Node with
                            | AstNode.Expression(SynExpr.IfThenElse(_)) ->
                                elseIfVisitor visitor depth visitorInfo checkFile
                            | _ ->
                                ContinueWithVisitor(visitor (depth + 1) visitorInfo checkFile)
                    | None -> 
                        Stop
            | _ -> Continue

    type RegisterNestedStatementsVisitor() = 
        let plugin =
            {
                Name = AnalyserName
                Visitor = Ast(visitor 0)
            }

        interface IRegisterPlugin with
            member __.RegisterPlugin with get() = plugin