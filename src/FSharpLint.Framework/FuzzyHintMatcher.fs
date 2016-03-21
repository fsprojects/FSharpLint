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

open HintParser
open MergeSyntaxTrees
open AbstractSyntaxArray

module FuzzyHintMatcher =        

    let inline isMatch (syntaxNode:AbstractSyntaxArray.Node) (hintNode:Edge) = 
        match hintNode, syntaxNode.SyntaxNode with
        | HashCodeLookupNode(SyntaxHintNode.Identifier, lookup), SyntaxNode.Identifier
        | HashCodeLookupNode(SyntaxHintNode.ConstantBool, lookup), SyntaxNode.ConstantBool
        | HashCodeLookupNode(SyntaxHintNode.ConstantByte, lookup), SyntaxNode.ConstantByte
        | HashCodeLookupNode(SyntaxHintNode.ConstantBytes, lookup), SyntaxNode.ConstantBytes
        | HashCodeLookupNode(SyntaxHintNode.ConstantChar, lookup), SyntaxNode.ConstantChar
        | HashCodeLookupNode(SyntaxHintNode.ConstantDecimal, lookup), SyntaxNode.ConstantDecimal
        | HashCodeLookupNode(SyntaxHintNode.ConstantDouble, lookup), SyntaxNode.ConstantDouble
        | HashCodeLookupNode(SyntaxHintNode.ConstantInt16, lookup), SyntaxNode.ConstantInt16
        | HashCodeLookupNode(SyntaxHintNode.ConstantInt32, lookup), SyntaxNode.ConstantInt32
        | HashCodeLookupNode(SyntaxHintNode.ConstantInt64, lookup), SyntaxNode.ConstantInt64
        | HashCodeLookupNode(SyntaxHintNode.ConstantIntPtr, lookup), SyntaxNode.ConstantIntPtr
        | HashCodeLookupNode(SyntaxHintNode.ConstantSByte, lookup), SyntaxNode.ConstantSByte
        | HashCodeLookupNode(SyntaxHintNode.ConstantSingle, lookup), SyntaxNode.ConstantSingle
        | HashCodeLookupNode(SyntaxHintNode.ConstantString, lookup), SyntaxNode.ConstantString
        | HashCodeLookupNode(SyntaxHintNode.ConstantUInt16, lookup), SyntaxNode.ConstantUInt16
        | HashCodeLookupNode(SyntaxHintNode.ConstantUInt32, lookup), SyntaxNode.ConstantUInt32
        | HashCodeLookupNode(SyntaxHintNode.ConstantUInt64, lookup), SyntaxNode.ConstantUInt64
        | HashCodeLookupNode(SyntaxHintNode.ConstantUIntPtr, lookup), SyntaxNode.ConstantUIntPtr -> 
            match lookup.TryGetValue syntaxNode.Identifier with
            | true, node -> Some(node)
            | false, _ -> None
        | AggreggatedNode(SyntaxHintNode.Null, node), SyntaxNode.Null
        | AggreggatedNode(SyntaxHintNode.Expression, node), SyntaxNode.Expression
        | AggreggatedNode(SyntaxHintNode.FuncApp, node), SyntaxNode.FuncApp
        | AggreggatedNode(SyntaxHintNode.Unit, node), SyntaxNode.Unit

        | AggreggatedNode(SyntaxHintNode.If, node), SyntaxNode.If

        | AggreggatedNode(SyntaxHintNode.Lambda, node), SyntaxNode.Lambda
        | AggreggatedNode(SyntaxHintNode.LambdaArg, node), SyntaxNode.LambdaArg
        | AggreggatedNode(SyntaxHintNode.LambdaBody, node), SyntaxNode.LambdaBody

        | AggreggatedNode(SyntaxHintNode.ArrayOrList, node), SyntaxNode.ArrayOrList
        | AggreggatedNode(SyntaxHintNode.Tuple, node), SyntaxNode.Tuple

        | AggreggatedNode(SyntaxHintNode.Wildcard, node), _
        | AggreggatedNode(SyntaxHintNode.Variable, node), _ -> Some(node)

        | _ -> None

    let possibleMatches (nodeArray:AbstractSyntaxArray.Node []) (skipArray:int []) (hintTrie:Edge list) notify = 
        assert (nodeArray.Length = skipArray.Length)

        let len = nodeArray.Length

        let rec checkTrie i hints notify =
            let syntaxNode = nodeArray.[i]
            let skip = skipArray.[i]

            let hints =
                hints 
                |> List.collect (fun hintEdge -> 
                    match isMatch syntaxNode hintEdge with 
                    | Some(matchedNode) -> 
                        for matchedHint in matchedNode.MatchedHint do 
                            notify syntaxNode matchedHint

                        matchedNode.Edges
                    | None -> [])

            if i + 1 < len && not hints.IsEmpty then
                checkTrie (i + 1) hints notify
            else
                ()

        let mutable i = 0
        while i < len do
            checkTrie i hintTrie (notify nodeArray.[i])
            i <- i + 1
