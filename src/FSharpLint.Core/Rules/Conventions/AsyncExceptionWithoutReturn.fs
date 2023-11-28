module FSharpLint.Rules.AsyncExceptionWithoutReturn

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner args =
    match args.AstNode with
    | AstNode.Expression(SynExpr.App (_, _, (SynExpr.Ident compExprName), (SynExpr.CompExpr (_, _, innerExpression, _)), range))
        when compExprName.idText = "async" ->
        match innerExpression with
        | SynExpr.Sequential(_, _, expression, _, _) -> 
            match expression with
            | SynExpr.App (_, _, SynExpr.Ident failwithId, _, _) when
                failwithId.idText = "failwith"
                || failwithId.idText = "failwithf"
                || failwithId.idText = "raise" ->
                    {   Range = range
                        Message = Resources.GetString "RulesAsyncExceptionWithoutReturn"
                        SuggestedFix = None
                        TypeChecks = List.Empty
                    } |> Array.singleton
            | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty

let rule =
    { Name = "AsyncExceptionWithoutReturn"
      Identifier = Identifiers.AsyncExceptionWithoutReturn
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
