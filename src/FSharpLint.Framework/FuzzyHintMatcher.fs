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

module FuzzyHintMatcher =        

    let inline isMatch (syntaxNode:AbstractSyntaxArray.Node) (hintNode:Edge) = 
        match hintNode.Lookup.TryGetValue syntaxNode.Hashcode with
        | true, node -> Some(node)
        | false, _ -> None

    let possibleMatches (nodeArray:AbstractSyntaxArray.Node []) (skipArray:int []) (hintTrie:Edge []) notify = 
        assert (nodeArray.Length = skipArray.Length)

        let len = nodeArray.Length

        let rec checkTrie i hints notify =
            let syntaxNode = nodeArray.[i]
            let skip = skipArray.[i]

            let hints =
                hints 
                |> Array.collect (fun hintEdge -> 
                    match isMatch syntaxNode hintEdge with 
                    | Some(matchedNode) -> 
                        for matchedHint in matchedNode.MatchedHint do 
                            notify syntaxNode matchedHint

                        matchedNode.Edges
                    | None -> [||])

            if i + 1 < len && hints.Length > 0 then
                checkTrie (i + 1) hints notify

        let mutable i = 0
        while i < len do
            checkTrie i hintTrie (notify nodeArray.[i])
            i <- i + 1
