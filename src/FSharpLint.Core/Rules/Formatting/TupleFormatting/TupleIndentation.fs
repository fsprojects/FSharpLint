module FSharpLint.Rules.TupleIndentation

open System
open System.Diagnostics.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

// Check that tuple items on separate lines have consistent indentation.
let checkTupleIndentation _ (tupleExprs:SynExpr list) _ _ =
    tupleExprs
    |> List.toArray
    |> Array.groupBy (fun expr -> expr.Range.StartLine)
    |> Array.choose (snd >> Array.tryHead)
    |> Array.pairwise
    |> Array.choose (fun (expr, nextExpr) ->
        if expr.Range.StartColumn <> nextExpr.Range.StartColumn then
            { Range = Range.mkRange "" expr.Range.Start nextExpr.Range.End
              Message = Resources.GetString("RulesFormattingTupleIndentationError")
              SuggestedFix = None
              TypeChecks = [] } |> Some
        else
            None)

let runner (args:AstNodeRuleParams) = TupleFormatting.isActualTuple args checkTupleIndentation

let rule =
    { Name = "TupleIndentation"
      Identifier = Identifiers.TupleIndentation
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule