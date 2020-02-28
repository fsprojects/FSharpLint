module FSharpLint.Rules.PatternMatchExpressionIndentation

open System
open FSharp.Compiler.Ast
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

let check (args:AstNodeRuleParams) _ (clauses:SynMatchClause list) _ =
    clauses
    |> List.toArray
    |> Array.choose (fun clause ->
        let (SynMatchClause.Clause (pat, guard, expr, _, _)) = clause
        let clauseIndentation = ExpressionUtilities.getLeadingSpaces clause.Range args.FileContent
        let exprIndentation = ExpressionUtilities.getLeadingSpaces expr.Range args.FileContent
        let matchPatternEndLine =
            guard
            |> Option.map (fun expr -> expr.Range.EndLine)
            |> Option.defaultValue pat.Range.EndLine
        if expr.Range.StartLine <> matchPatternEndLine && exprIndentation <> clauseIndentation + args.globalConfig.numIndentationSpaces then
            { Range = expr.Range
              Message = Resources.GetString("RulesFormattingMatchExpressionIndentationError")
              SuggestedFix = None
              TypeChecks = [] } |> Some
        else
            None)

let runner (args:AstNodeRuleParams) = PatternMatchFormatting.isActualPatternMatch args check

let rule =
    { Name = "PatternMatchExpressionIndentation"
      Identifier = Identifiers.PatternMatchExpressionIndentation
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
