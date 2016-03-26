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

    let rec checkTrie i trie (nodeArray:AbstractSyntaxArray.Node []) (skipArray:int []) notify =
        trie.MatchedHint |> List.iter notify

        if i < nodeArray.Length then
            let node = nodeArray.[i]

            match trie.Edges.Lookup.TryGetValue node.Hashcode with
            | true, trie -> checkTrie (i + 1) trie nodeArray skipArray notify
            | false, _ -> ()

        trie.Edges.AnyMatch 
        |> List.iter (fun (var, trie) -> 
            match var with 
            | Some(var) -> () 
            | None -> checkTrie (i + skipArray.[i] + 1) trie nodeArray skipArray notify)

    let possibleMatches (nodeArray:AbstractSyntaxArray.Node []) (skipArray:int []) (hintTrie:Edges) notify = 
        assert (nodeArray.Length = skipArray.Length)

        let len = nodeArray.Length

        let mutable i = 0
        while i < len do
            let node = nodeArray.[i]
            
            match hintTrie.Lookup.TryGetValue node.Hashcode with
            | true, trie -> checkTrie (i + 1) trie nodeArray skipArray (notify node)
            | false, _ -> ()

            i <- i + 1
