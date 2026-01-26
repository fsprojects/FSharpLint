module FSharpLint.Rules.FavourNamesInDUMembers

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Field(SynField(_, false, None, _, false, _, _, range, _)) -> 
        Array.singleton 
            { 
                Range = range
                Message = Resources.GetString "RulesFavourNamesInDUMembers"
                SuggestedFix = None
                TypeChecks = List.Empty 
            }
    | _ -> Array.empty

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
