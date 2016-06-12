// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.Rules

module NestedStatements =
    
    open System
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "NestedStatements"
    
    let private configDepth config =
        match isAnalyserEnabled config AnalyserName with
        | Some(analyser) ->
            match Map.tryFind "Depth" analyser.Settings with
            | Some(Depth(l)) -> Some(l)
            | Some(_) | None -> None
        | Some(_) | None -> None

    let private error (depth:int) =
        let errorFormatString = Resources.GetString("RulesNestedStatementsError")
        String.Format(errorFormatString, depth)

    /// Lambda wildcard arguments are named internally as _argN, a match is then generated for them in the AST.
    /// e.g. fun _ -> () is represented in the AST as fun _arg1 -> match _arg1 with | _ -> ().
    /// This function returns true if the given match statement is compiler generated for a lmabda wildcard argument.
    let private isCompilerGeneratedMatch = function
        | SynExpr.Match(_, SynExpr.Ident(ident), _, _, _) when ident.idText.StartsWith("_arg") -> true
        | _ -> false

    let private areChildrenNested = function
        | AstNode.Binding(SynBinding.Binding(_))
        | AstNode.Expression(SynExpr.Lambda(_))
        | AstNode.Expression(SynExpr.MatchLambda(_))
        | AstNode.Expression(SynExpr.IfThenElse(_))
        | AstNode.Expression(SynExpr.Lazy(_))
        | AstNode.Expression(SynExpr.Record(_))
        | AstNode.Expression(SynExpr.ObjExpr(_))
        | AstNode.Expression(SynExpr.TryFinally(_))
        | AstNode.Expression(SynExpr.TryWith(_))
        | AstNode.Expression(SynExpr.Tuple(_))
        | AstNode.Expression(SynExpr.Quote(_))
        | AstNode.Expression(SynExpr.While(_))
        | AstNode.Expression(SynExpr.For(_))
        | AstNode.Expression(SynExpr.ForEach(_)) -> true
        | AstNode.Expression(SynExpr.Match(_) as matchExpr) when not (isCompilerGeneratedMatch matchExpr) -> true
        | _ -> false

    let private getRange = function 
        | AstNode.Expression(node) -> Some node.Range
        | AstNode.Binding(node) -> Some node.RangeOfBindingAndRhs
        | _ -> None

    let private distanceToCommonParent (syntaxArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) i j =
        let mutable i = i
        let mutable j = j
        let mutable distance = 0

        while i <> j do
            if i > j then
                if areChildrenNested syntaxArray.[i].Actual then
                    distance <- distance + 1

                i <- skipArray.[i].ParentIndex
            else
                j <- skipArray.[j].ParentIndex

        distance

    let analyser visitorInfo _ (syntaxArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) = 
        let isSuppressed i =
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName AbstractSyntaxArray.SuppressRuleWildcard

        match configDepth visitorInfo.Config with
        | Some(errorDepth) ->
            let error = error errorDepth

            /// Is node a duplicate of a node in the AST containing ExtraSyntaxInfo 
            /// e.g. lambda arg being a duplicate of the lamdba.
            let isMetaData node i =
                let parentIndex = skipArray.[i].ParentIndex
                if parentIndex = i then false
                else
                    Object.ReferenceEquals(node, syntaxArray.[parentIndex].Actual)

            let isElseIf node i =
                match node with
                | AstNode.Expression(SynExpr.IfThenElse(_)) ->
                    let parentIndex = skipArray.[i].ParentIndex
                    if parentIndex = i then false
                    else
                        match syntaxArray.[parentIndex].Actual with
                        | AstNode.Expression(SynExpr.IfThenElse(_, _, Some(_), _, _, _, _)) -> true
                        | _ -> false
                | _ -> false

            let mutable depth = 0
            let mutable i = 0
            while i < syntaxArray.Length do
                if i + 1 < syntaxArray.Length then
                    // If next node in array is not a sibling or child of the current node.
                    let parent = skipArray.[i + 1].ParentIndex
                    if parent <> i && parent <> skipArray.[i].ParentIndex then
                        // Decrement depth until we reach a common parent.
                        depth <- depth - (distanceToCommonParent syntaxArray skipArray i (i + 1))

                let node = syntaxArray.[i].Actual

                if areChildrenNested node && not <| isMetaData node i && not <| isElseIf node i then
                    if depth >= errorDepth then
                        if not (isSuppressed i) then
                            getRange node |> Option.iter (fun range -> visitorInfo.PostError range error)
                    
                        // Skip children as we've had an error containing them.
                        i <- i + skipArray.[i].NumberOfChildren
                    else
                        depth <- depth + 1

                i <- i + 1
        | None -> ()