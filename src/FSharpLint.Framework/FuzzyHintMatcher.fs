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
open System.Collections.Generic

module FuzzyHintMatcher =

    let isMatch i j (nodeArray:AbstractSyntaxArray.Node []) (skipArray:int []) = 
        let skipI = skipArray.[i]
        let skipJ = skipArray.[j]

        if skipI = skipJ then
            Array.zip [|i..i + skipI|] [|j..j + skipJ|]
            |> Array.forall (fun (i, j) -> 
                i < nodeArray.Length && 
                j < nodeArray.Length && 
                nodeArray.[i].Hashcode = nodeArray.[j].Hashcode)
        else false

    let rec checkTrie i trie (nodeArray:AbstractSyntaxArray.Node []) (skipArray:int []) (boundVariables:Dictionary<_, _>) notify =
        trie.MatchedHint |> List.iter notify

        if i < nodeArray.Length then
            let node = nodeArray.[i]

            match trie.Edges.Lookup.TryGetValue node.Hashcode with
            | true, trie -> checkTrie (i + 1) trie nodeArray skipArray boundVariables notify
            | false, _ -> ()

        trie.Edges.AnyMatch 
        |> List.iter (fun (var, trie) -> 
            match var with 
            | Some(var) -> 
                match boundVariables.TryGetValue var with 
                | true, varI when isMatch varI i nodeArray skipArray  -> 
                    checkTrie (i + skipArray.[i] + 1) trie nodeArray skipArray boundVariables notify
                | false, _ -> 
                    boundVariables.Add(var, i)
                    checkTrie (i + skipArray.[i] + 1) trie nodeArray skipArray boundVariables notify
                | true, _ -> ()
            | None -> checkTrie (i + skipArray.[i] + 1) trie nodeArray skipArray boundVariables notify)

    let possibleMatches (nodeArray:AbstractSyntaxArray.Node []) (skipArray:int []) (hintTrie:Edges) notify = 
        assert (nodeArray.Length = skipArray.Length)

        let len = nodeArray.Length

        let mutable i = 0
        while i < len do
            let node = nodeArray.[i]
            
            match hintTrie.Lookup.TryGetValue node.Hashcode with
            | true, trie -> checkTrie (i + 1) trie nodeArray skipArray (Dictionary<_, _>()) (notify node)
            | false, _ -> ()

            i <- i + 1
