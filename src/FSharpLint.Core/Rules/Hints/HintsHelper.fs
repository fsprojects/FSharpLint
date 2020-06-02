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
module internal FSharpLint.Rules.Helper.Hints

open System.Collections.Generic
open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework
open FSharpLint.Framework.Ast
open FSharpLint.Framework.HintParser
open MergeSyntaxTrees

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
let rec checkTrie i trie (nodeArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) (boundVariables:Dictionary<_, _>) notify =
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

