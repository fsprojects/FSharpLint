module FSharpLint.Rules.TupleParentheses

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

let checkTupleHasParentheses (args:AstNodeRuleParams) _ range parentNode =
    let processText text =
        let autoFix =
            lazy
                (Some 
                    {
                        FromRange = range
                        FromText = text
                        ToText = $"({text})"
                     })

        {
            Range = range
            Message = Resources.GetString "RulesFormattingTupleParenthesesViolation"
            AutoFix = Some autoFix
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
