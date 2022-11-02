module FSharpLint.Rules.AsyncIgnoreWithType

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Suggestion

let runner (args:AstNodeRuleParams) =
    let result = 
        match args.AstNode with
        | AstNode.Expression(SynExpr.DoBang(expr, _)) ->
            match expr with
            | SynExpr.App(_, _, _, ignoreExpr, _) ->
                match ignoreExpr with
                | SynExpr.LongIdent(_,LongIdentWithDots(ident, _), _, range) ->
                    if ident.[0].ToString().Equals("Async") 
                        && ident.[1].ToString().Equals("Ignore") then
                        { Range = range
                          Message = ""
                          SuggestedFix = None
                          TypeChecks = List.Empty } |> Array.singleton
                    else 
                        Array.empty
                | _ -> Array.empty
            | _ -> 
                Array.empty
        | _ ->
            Array.empty
    result

let rule =
    { Name = "AsyncIgnoreWithType"
      Identifier = Identifiers.AvoidTooShortNames
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
