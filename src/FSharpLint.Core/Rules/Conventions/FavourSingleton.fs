module FSharpLint.Rules.FavourSingleton

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

let runner args =
    let generateViolation range =
        let msg = Resources.GetString "RulesFavourSingleton"
        Array.singleton
            { Range = range
              Message = msg
              SuggestedFix = None
              TypeChecks = List.Empty }
    match args.AstNode with
    | AstNode.Expression(expression) ->
        match expression with
        | SynExpr.ArrayOrListComputed(_isArray, innerExpr, _) ->
            match innerExpr with
            | SynExpr.ComputationExpr _
            | SynExpr.For _ 
            | SynExpr.ForEach _ 
            | SynExpr.IfThenElse _ 
            | SynExpr.IndexRange _
            | SynExpr.LetOrUse _
            | SynExpr.Match _
            | SynExpr.Sequential _
            | SynExpr.SequentialOrImplicitYield _
            | SynExpr.Set _ 
            | SynExpr.TryFinally _
            | SynExpr.TryWith _
            | SynExpr.While _
            | SynExpr.YieldOrReturn _
            | SynExpr.YieldOrReturnFrom _ ->
                Array.empty
            | _ ->
                generateViolation expression.Range
        | _ -> Array.empty
    | _ -> Array.empty
let rule =
    AstNodeRule
        { Name = "FavourSingleton"
          Identifier = Identifiers.FavourSingleton
          RuleConfig =
              { AstNodeRuleConfig.Runner = runner
                Cleanup = ignore } }
