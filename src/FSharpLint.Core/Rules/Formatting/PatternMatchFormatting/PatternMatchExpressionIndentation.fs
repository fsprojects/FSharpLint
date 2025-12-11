module FSharpLint.Rules.PatternMatchExpressionIndentation

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

let check (args:AstNodeRuleParams) _ (clauses:SynMatchClause list) _ =
    let processClause clause =
        let (SynMatchClause (pat, guard, expr, _, _, _)) = clause
        let clauseIndentation = ExpressionUtilities.getLeadingSpaces clause.Range args.FileContent
        let exprIndentation = ExpressionUtilities.getLeadingSpaces expr.Range args.FileContent
        let matchPatternEndLine =
            guard
            |> Option.map (fun expr -> expr.Range.EndLine)
            |> Option.defaultValue pat.Range.EndLine
        if expr.Range.StartLine <> matchPatternEndLine && exprIndentation <> clauseIndentation + args.GlobalConfig.numIndentationSpaces then
            Some
                {
                    Range = expr.Range
                    Message = Resources.GetString "RulesFormattingMatchExpressionIndentationViolation"
                    AutoFix = None
                    TypeChecks = List.Empty
                }
        else
            None

    clauses
    |> List.toArray
    |> Array.choose processClause

let runner (args:AstNodeRuleParams) = PatternMatchFormatting.isActualPatternMatch args check

let rule =
    AstNodeRule
        {
            Name = "PatternMatchExpressionIndentation"
            Identifier = Identifiers.PatternMatchExpressionIndentation
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
