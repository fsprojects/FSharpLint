module FSharpLint.Rules.PatternMatchClauseIndentation

open System
open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

[<RequireQualifiedAccess>]
type Config = { AllowSingleLineLambda:bool }

let check (config:Config) (args:AstNodeRuleParams) matchExprRange (clauses:SynMatchClause list) isLambda =
    let matchStartIndentation = ExpressionUtilities.getLeadingSpaces matchExprRange args.FileContent

    let indentationLevelError =
        if isLambda && config.AllowSingleLineLambda && clauses |> List.forall (fun clause -> clause.Range.StartLine = matchExprRange.StartLine) then
            None
        else
            clauses
            |> List.tryHead
            |> Option.bind (fun firstClause ->
                let clauseIndentation = ExpressionUtilities.getLeadingSpaces firstClause.Range args.FileContent
                if isLambda then
                    if clauseIndentation <> matchStartIndentation + args.GlobalConfig.NumIndentationSpaces then
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
        |> Array.choose (fun ((_, clauseOneSpaces), (clauseTwo, clauseTwoSpaces)) ->
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

let runner (config:Config) (args:AstNodeRuleParams) = PatternMatchFormatting.isActualPatternMatch args (check config)

let rule config =
    { Name = "PatternMatchClauseIndentation"
      Identifier = Identifiers.PatternMatchClauseIndentation
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule
