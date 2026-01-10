module FSharpLint.Rules.TupleCommaSpacing

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

// Check for single space after commas in tuple.
let checkTupleCommaSpacing (args:AstNodeRuleParams) (tupleExprs:SynExpr list) _tupleRange _ =
    let choose (expr: SynExpr) (nextExpr: SynExpr) = 
      if expr.Range.EndLine = nextExpr.Range.StartLine && expr.Range.EndColumn + 2 <> nextExpr.Range.StartColumn then
        let commaRange = Range.mkRange String.Empty expr.Range.End nextExpr.Range.Start
        let map commaText =
            lazy
                (Some
                    {
                        FromRange = commaRange
                        FromText = commaText
                        ToText = ", "
                    })
        let suggestedFix =
            ExpressionUtilities.tryFindTextOfRange commaRange args.FileContent
            |> Option.map map

        Some
            {
                Range = commaRange
                Message = Resources.GetString("RulesFormattingTupleCommaSpacingError")
                SuggestedFix = suggestedFix
                TypeChecks = List.Empty
            }
      else
          None

    tupleExprs
    |> List.toArray
    |> Array.pairwise
    |> Array.choose (fun (expr, nextExpr) -> choose expr nextExpr)

let runner (args:AstNodeRuleParams) = TupleFormatting.isActualTuple args checkTupleCommaSpacing

let rule =
    AstNodeRule
        {
            Name = "TupleCommaSpacing"
            Identifier = Identifiers.TupleCommaSpacing
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
