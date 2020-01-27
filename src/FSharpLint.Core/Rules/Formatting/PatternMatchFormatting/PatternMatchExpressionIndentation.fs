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
        let clauseIndentation = ExpressionUtilities.getLeadingSpaces clause.Range args.fileContent
        let exprIndentation = ExpressionUtilities.getLeadingSpaces expr.Range args.fileContent
        let matchPatternEndLine =
            guard
            |> Option.map (fun expr -> expr.Range.EndLine)
            |> Option.defaultValue pat.Range.EndLine
        if expr.Range.StartLine <> matchPatternEndLine && exprIndentation <> clauseIndentation + 4 then
            { Range = expr.Range
              Message = Resources.GetString("RulesFormattingMatchExpressionIndentationError")
              SuggestedFix = None
              TypeChecks = [] } |> Some
        else
            None)

let runner (args:AstNodeRuleParams) = PatternMatchFormatting.isActualPatternMatch args check

let rule =
    { name = "PatternMatchExpressionIndentation"
      identifier = Identifiers.PatternMatchExpressionIndentation
      ruleConfig = { AstNodeRuleConfig.runner = runner; cleanup = ignore } }
    |> AstNodeRule
