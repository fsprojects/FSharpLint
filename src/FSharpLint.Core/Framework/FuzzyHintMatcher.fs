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

namespace FSharpLint.Framework

open HintParser
open MergeSyntaxTrees
open System.Collections.Generic

/// Matching hints is done in two passes: fuzzy match, and an untyped ast match.
/// The untyped ast match attempts to match a single hint against a given node in the ast,
/// to avoid attempting every hint against every node, an initial pass (the fuzzy match) is done
/// to eliminate as many cases where there'll never be a match as quickly as possible, so that the
/// ast match is run against as few hints and ast nodes as possible.
/// 
/// The fuzzy match requires two structures to be computed before hand: an abstract syntax array 
/// constructed from the ast, and a trie of hints. Both of these structures contain hash codes of the 
/// nodes, the hash codes are expected to match when the nodes are equivalent. The matching is done using these
/// hash codes so we end up with a trie of integers searching against an array of integers -
/// which is pretty fast.
module FuzzyHintMatcher =

    open Microsoft.FSharp.Compiler.Ast

    open Ast

    /// Confirms if two parts of the ast look alike.
    /// This is required as hints can bind variables: the bound location needs to be compared to
    /// parts of the ast that the hint covers with the same variable.
    let private isMatch i j (nodeArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) = 
        let skipI = skipArray.[i].NumberOfChildren
        let skipJ = skipArray.[j].NumberOfChildren

        if skipI = skipJ then
            Array.zip [|i..i + skipI|] [|j..j + skipJ|]
            |> Array.forall (fun (i, j) -> 
                i < nodeArray.Length && 
                j < nodeArray.Length && 
                nodeArray.[i].Hashcode = nodeArray.[j].Hashcode)
        else false

    let inline private isParen (node:AbstractSyntaxArray.Node) =
        match node.Actual with
        | AstNode.Expression(SynExpr.Paren(_)) -> true
        | _ -> false

    /// Compares the hint trie against a given location in the abstract syntax array.
    let rec private checkTrie i trie (nodeArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) (boundVariables:Dictionary<_, _>) notify =
        trie.MatchedHint |> List.iter notify

        if i < nodeArray.Length then
            let node = nodeArray.[i]

            if isParen node then
                // Skip the paren.
                checkTrie (i + 1) trie nodeArray skipArray boundVariables notify
            else
                match trie.Edges.Lookup.TryGetValue node.Hashcode with
                | true, trie -> checkTrie (i + 1) trie nodeArray skipArray boundVariables notify
                | false, _ -> ()

            trie.Edges.AnyMatch 
            |> List.iter (fun (var, trie) -> 
                match var with 
                | Some(var) -> 
                    match boundVariables.TryGetValue var with 
                    | true, varI when isMatch varI i nodeArray skipArray  -> 
                        checkTrie (i + skipArray.[i].NumberOfChildren + 1) trie nodeArray skipArray boundVariables notify
                    | false, _ -> 
                        boundVariables.Add(var, i)
                        checkTrie (i + skipArray.[i].NumberOfChildren + 1) trie nodeArray skipArray boundVariables notify
                    | true, _ -> ()
                | None -> checkTrie (i + skipArray.[i].NumberOfChildren + 1) trie nodeArray skipArray boundVariables notify)

    /// Searches the abstract syntax array for possible hint matches using the hint trie.
    /// Any possible matches that are found will be given to the callback function `notify`,
    /// any matches found are not guaranteed and it's expected that the caller verify the match.
    let possibleMatches (nodeArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) (hintTrie:Edges) notify = 
        assert (nodeArray.Length = skipArray.Length)

        let len = nodeArray.Length

        let mutable i = 0
        while i < len do
            let node = nodeArray.[i]
            
            match hintTrie.Lookup.TryGetValue node.Hashcode with
            | true, trie -> checkTrie (i + 1) trie nodeArray skipArray (Dictionary<_, _>()) (notify i)
            | false, _ -> ()

            i <- i + 1
