module FSharpLint.Rules.TupleParentheses

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

let checkTupleHasParentheses (args:AstNodeRuleParams) _ range parentNode =
    let processText text =
        let fix =
            lazy
                (Some 
                    {
                        FromRange = range
                        ToText = $"({text})"
                     })

        {
            Range = range
            Message = Resources.GetString("RulesFormattingTupleParenthesesError")
            Fix = Some fix
            TypeChecks = List.Empty
        }

    match parentNode with
    | Some (AstNode.Expression (SynExpr.Paren _)) ->
        Array.empty
    | _ ->
        ExpressionUtilities.tryFindTextOfRange range args.FileContent
        |> Option.map processText
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
