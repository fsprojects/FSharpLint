module FSharpLint.Rules.PatternMatchClauseIndentation

open System
open FSharp.Compiler.Ast
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

let check (args:AstNodeRuleParams) matchExprRange (clauses:SynMatchClause list) isLambda =
    let matchStartIndentation = ExpressionUtilities.getLeadingSpaces matchExprRange args.FileContent

    let indentationLevelError =
        clauses
        |> List.tryHead
        |> Option.bind (fun firstClause ->
            let clauseIndentation = ExpressionUtilities.getLeadingSpaces firstClause.Range args.FileContent
            if isLambda then
                if clauseIndentation <> matchStartIndentation + 4 then
                    { Range = firstClause.Range
                      Message = Resources.GetString("RulesFormattingLambdaPatternMatchClauseIndentationError")
                      SuggestedFix = None
                      TypeChecks = [] } |> Some
                else
                    None
            elif clauseIndentation <> matchStartIndentation then
                { Range = firstClause.Range
                  Message = Resources.GetString("RulesFormattingPatternMatchClauseIndentationError")
                  SuggestedFix = None
                  TypeChecks = [] } |> Some
            else
                None)

    let consistentIndentationErrors =
        clauses
        |> List.toArray
        |> Array.map (fun clause -> (clause, ExpressionUtilities.getLeadingSpaces clause.Range args.FileContent))
        |> Array.pairwise
        |> Array.choose (fun ((clauseOne, clauseOneSpaces), (clauseTwo, clauseTwoSpaces)) ->
            if clauseOneSpaces <> clauseTwoSpaces then
                { Range = clauseTwo.Range
                  Message = Resources.GetString("RulesFormattingPatternMatchClauseSameIndentationError")
                  SuggestedFix = None
                  TypeChecks = [] } |> Some
            else
                None)

    [|
        indentationLevelError |> Option.toArray
        consistentIndentationErrors
    |]
    |> Array.concat

let runner (args:AstNodeRuleParams) = PatternMatchFormatting.isActualPatternMatch args check

let rule =
    { Name = "PatternMatchClauseIndentation"
      Identifier = Identifiers.PatternMatchClauseIndentation
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
