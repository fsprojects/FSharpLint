module FSharpLint.Rules.Helper.RaiseWithTooManyArguments

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private (|RaiseWithTooManyArgs|_|) identifier maxArgs = function
    | SynExpr.Ident(ident)::arguments when List.length arguments > maxArgs && ident.idText = identifier ->
        Some()
    | _ -> None

let checkRaiseWithTooManyArgs (raiseType:string) (count:int) (ruleName:string) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.App(_, false, _, _, _)) as expr ->
        match expr with
        | FuncApp(expressions, range) ->
            match expressions with
            | RaiseWithTooManyArgs raiseType count ->
                {
                    Range = range
                    Message = Resources.GetString ruleName
                    SuggestedFix = None
                    TypeChecks = []
                } |> Array.singleton
            | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty
