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
module FSharpLint.Rules.Helper.Hints

open System.Collections.Generic
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Ast
open FSharpLint.Framework.HintParser
open MergeSyntaxTrees

/// Confirms if two parts of the ast look alike.
/// This is required as hints can bind variables: the bound location needs to be compared to
/// parts of the ast that the hint covers with the same variable.
let private isMatch iIndex jIndex (nodeArray:AbstractSyntaxArray.Node []) = 
    let numChildrenI = nodeArray.[iIndex].NumberOfChildren
    let numChildrenJ = nodeArray.[jIndex].NumberOfChildren

    if numChildrenI = numChildrenJ then
        let numChildren = numChildrenI

        Seq.forall (fun child -> 
            iIndex + child < nodeArray.Length && 
            jIndex + child < nodeArray.Length && 
            nodeArray.[iIndex + child].Hashcode = nodeArray.[jIndex + child].Hashcode) { 0..numChildren }
    else false

let inline private isParen (node:AbstractSyntaxArray.Node) =
    match node.Actual with
    | AstNode.Expression(SynExpr.Paren(_)) -> true
    | _ -> false

// hard to turn into tail-recursive form
// fsharplint:disable EnsureTailCallDiagnosticsInRecursiveFunctions
/// Compares the hint trie against a given location in the abstract syntax array.
let rec checkTrie index trie (nodeArray:AbstractSyntaxArray.Node []) (boundVariables:Dictionary<_, _>) notify =
    List.iter notify trie.MatchedHint

    if index < nodeArray.Length then
        let node = nodeArray.[index]

        if isParen node then
            // Skip the paren.
            checkTrie (index + 1) trie nodeArray boundVariables notify
        else
            match trie.Edges.Lookup.TryGetValue node.Hashcode with
            | true, trie -> checkTrie (index + 1) trie nodeArray boundVariables notify
            | false, _ -> ()

        let collect var trie = 
            match var with
            | Some(var) -> 
                match boundVariables.TryGetValue var with
                | true, varI when isMatch varI index nodeArray  -> 
                    checkTrie (index + node.NumberOfChildren + 1) trie nodeArray boundVariables notify
                | false, _ -> 
                    boundVariables.Add(var, index)
                    checkTrie (index + node.NumberOfChildren + 1) trie nodeArray boundVariables notify
                | true, _ -> ()
            | None -> checkTrie (index + node.NumberOfChildren + 1) trie nodeArray boundVariables notify

        List.iter (fun (var, trie) -> collect var trie) trie.Edges.AnyMatch
// fsharplint:enable EnsureTailCallDiagnosticsInRecursiveFunctions
