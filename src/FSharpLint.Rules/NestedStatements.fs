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
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.AstInfo
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadAnalysers

    [<Literal>]
    let AnalyserName = "FSharpLint.NestedStatements"

    let configDepth (config:Map<string,Analyser>) =
        match isAnalyserEnabled config AnalyserName with
            | Some(analyserSettings) when analyserSettings.ContainsKey "Depth" ->
                match analyserSettings.["Depth"] with
                    | Depth(l) -> Some(l)
                    | _ -> None
            | Some(_)
            | None -> None

    let error depth =
        sprintf "Code should not be nested more deeply than a depth of %d" depth

    exception UnexpectedNodeTypeException of string
    
    let rec visitor depth (visitorInfo:VisitorInfo) (checkFile:CheckFileResults) astNode = 
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
            | AstNode.Expression(SynExpr.ForEach(_)) as node -> 
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
                        ContinueWithVisitor(visitor (depth + 1) visitorInfo checkFile)
                    | None -> 
                        Stop
            | _ -> Continue

    type RegisterXmlDocumentationAnalyser() = 
        let plugin =
            {
                Name = AnalyserName
                Analyser = Ast(visitor 0)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin