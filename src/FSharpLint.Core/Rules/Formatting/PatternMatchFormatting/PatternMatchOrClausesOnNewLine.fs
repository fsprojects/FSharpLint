module FSharpLint.Rules.PatternMatchOrClausesOnNewLine

open System
open FSharp.Compiler.Ast
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

let check args _ (clauses:SynMatchClause list) _ =
    clauses
    |> List.toArray
    |> Array.collect (function
        | SynMatchClause.Clause (SynPat.Or (firstPat, secondPat, _), _, _, _, _) ->
            [|firstPat; secondPat|]
        | _ -> [||])
    |> Array.pairwise
    |> Array.choose (fun (clauseOne, clauseTwo) ->
        if clauseOne.Range.EndLine = clauseTwo.Range.StartLine then
            { Range = clauseTwo.Range
              Message = Resources.GetString("RulesFormattingPatternMatchOrClausesOnNewLineError")
              SuggestedFix = None
              TypeChecks = [] } |> Some
        else
            None)

let runner (args:AstNodeRuleParams) = PatternMatchFormatting.isActualPatternMatch args check

let rule =
    { Name = "PatternMatchOrClausesOnNewLine"
      Identifier = Identifiers.PatternMatchOrClausesOnNewLine
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
