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
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, _, _, expression, _, _, _)) ->
        match expression with
        | SynExpr.ArrayOrListComputed(_isArray, innerExpr, range) ->
            match innerExpr with
            | SynExpr.Const(_, range) ->
              generateViolation range
            | SynExpr.Ident _ ->
              generateViolation range
            | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty
let rule =
    AstNodeRule
        { Name = "FavourSingleton"
          Identifier = Identifiers.FavourSingleton
          RuleConfig =
              { AstNodeRuleConfig.Runner = runner
                Cleanup = ignore } }
