module FSharpLint.Rules.FavourNamesInDUMembers

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | _node -> failwith "Not yet implemented"

let rule =
    AstNodeRule
        {
            Name = "FavourNamesInDUMembers"
            Identifier = Identifiers.FavourNamesInDUMembers
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
