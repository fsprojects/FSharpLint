module FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities

let checkIfInLibrary (syntaxArray: array<AbstractSyntaxArray.Node>) (range: range) : array<WarningDetails> =
    failwith "Not implemented"

let runner args =
    match args.AstNode with
    | AstNode.Identifier(["Async"; "RunSynchronously"], range) ->
        checkIfInLibrary args.SyntaxArray range
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "NoAsyncRunSynchronouslyInLibrary"
            Identifier = Identifiers.NoAsyncRunSynchronouslyInLibrary
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
