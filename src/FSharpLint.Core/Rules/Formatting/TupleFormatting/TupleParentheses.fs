module FSharpLint.Rules.TupleParentheses

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

let checkTupleHasParentheses (args:AstNodeRuleParams) _ range parentNode =
    let map text = 
        let suggestedFix =
            lazy
                (Some
                    {
                        FromRange = range
                        FromText = text
                        ToText = "(" + text + ")"
                    })

        {
            Range = range
            Message = Resources.GetString("RulesFormattingTupleParenthesesError")
            SuggestedFix = Some suggestedFix
            TypeChecks = List.Empty
        }

    match parentNode with
    | Some (AstNode.Expression (SynExpr.Paren _)) ->
        Array.empty
    | _ ->
        ExpressionUtilities.tryFindTextOfRange range args.FileContent
        |> Option.map map
        |> Option.toArray

let runner (args:AstNodeRuleParams) = TupleFormatting.isActualTuple args checkTupleHasParentheses

let rule =
    AstNodeRule
        {
            Name = "TupleParentheses"
            Identifier = Identifiers.TupleParentheses
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
