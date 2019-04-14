module FSharpLint.Rules.TupleCommaSpacing

open System
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

// Check for single space after commas in tuple.
let checkTupleCommaSpacing (args : AstNodeRuleParams) (tupleExprs : SynExpr list) tupleRange _ =
    tupleExprs
    |> List.toArray
    |> Array.pairwise
    |> Array.choose (fun (expr, nextExpr) ->
        if expr.Range.EndLine = nextExpr.Range.StartLine && expr.Range.EndColumn + 2 <> nextExpr.Range.StartColumn then
            let commaRange = mkRange "" expr.Range.End nextExpr.Range.Start
            let suggestedFix =
                ExpressionUtilities.tryFindTextOfRange commaRange args.fileContent
                |> Option.map (fun commaText ->
                    lazy(
                        { FromRange = commaRange
                          FromText = commaText
                          ToText = ", " } |> Some
                        ) )
            { Range = commaRange
              Message = Resources.GetString("RulesFormattingTupleCommaSpacingError")
              SuggestedFix = suggestedFix
              TypeChecks = [] } |> Some
        else
            None)
    
let runner (args : AstNodeRuleParams) = TupleFormatting.isActualTuple args checkTupleCommaSpacing
    
let rule =
    { name = "TupleCommaSpacing" 
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner; cleanup = ignore } }
    |> AstNodeRule
