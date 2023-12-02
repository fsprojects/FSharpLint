module FSharpLint.Rules.FavourSingleton

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

let runner args =
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, _, _, expression, _, _)) ->
        match expression with
        | SynExpr.ArrayOrListOfSeqExpr(_, SynExpr.CompExpr(_, _, expr,range), _) ->
            match expr with
            | SynExpr.Const(_, range) ->
                { Range = range
                  Message = String.Format(Resources.GetString "RulesFavourSingleton")
                  SuggestedFix = None
                  TypeChecks = List.Empty }
                  |> Array.singleton
            | SynExpr.Ident _ ->
              { Range = range
                Message = String.Format(Resources.GetString "RulesFavourSingleton")
                SuggestedFix = None
                TypeChecks = List.Empty }
                |> Array.singleton
              | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty
let rule =
    { Name = "FavourSingleton"
      Identifier = Identifiers.FavourSingleton
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
