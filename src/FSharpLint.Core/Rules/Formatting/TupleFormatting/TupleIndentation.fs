module FSharpLint.Rules.TupleIndentation

open System
open System.Diagnostics.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

// Check that tuple items on separate lines have consistent indentation.
let checkTupleIndentation _ (tupleExprs:SynExpr list) _ _ =
    let choose (expr: SynExpr) (nextExpr: SynExpr) =
        if expr.Range.StartColumn <> nextExpr.Range.StartColumn then
            Some
                {
                    Range = Range.mkRange "" expr.Range.Start nextExpr.Range.End
                    Message = Resources.GetString "RulesFormattingTupleIndentationViolation"
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }
        else
            None

    tupleExprs
    |> List.toArray
    |> Array.groupBy (fun expr -> expr.Range.StartLine)
    |> Array.choose (snd >> Array.tryHead)
    |> Array.pairwise
    |> Array.choose (fun (expr, nextExpr) -> choose expr nextExpr)

let runner (args:AstNodeRuleParams) = TupleFormatting.isActualTuple args checkTupleIndentation

let rule =
    AstNodeRule
        {
            Name = "TupleIndentation"
            Identifier = Identifiers.TupleIndentation
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
