module FSharpLint.Rules.PatternMatchOrClausesOnNewLine

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

let check args _ (clauses:SynMatchClause list) _ =
    let choose (clauseOne: SynPat) (clauseTwo: SynPat) = 
        if clauseOne.Range.EndLine = clauseTwo.Range.StartLine then
            Some
                {
                    Range = clauseTwo.Range
                    Message = Resources.GetString "RulesFormattingPatternMatchOrClausesOnNewLineViolation"
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }
        else
            None

    clauses
    |> List.toArray
    |> Array.collect (function
        | SynMatchClause (SynPat.Or (firstPat, secondPat, _, _), _, _, _, _, _) ->
            [|firstPat; secondPat|]
        | _ -> Array.empty)
    |> Array.pairwise
    |> Array.choose (fun (clauseOne, clauseTwo) -> choose clauseOne clauseTwo)

let runner (args:AstNodeRuleParams) = PatternMatchFormatting.isActualPatternMatch args check

let rule =
    AstNodeRule
        {
            Name = "PatternMatchOrClausesOnNewLine"
            Identifier = Identifiers.PatternMatchOrClausesOnNewLine
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
