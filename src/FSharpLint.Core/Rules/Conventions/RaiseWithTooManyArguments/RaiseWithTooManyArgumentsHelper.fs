module FSharpLint.Rules.Helper.RaiseWithTooManyArguments

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private (|RaiseWithTooManyArgs|_|) identifier maxArgs = function
    | ExpressionUtilities.Identifier([ ident ], _)::arguments 
        when List.length arguments > maxArgs && ident.idText = identifier ->
        Some()
    | _ -> None

let checkRaiseWithTooManyArgs (raiseType:string) (count:int) (ruleName:string) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.App(_, false, _, _, _)) as expr ->
        match expr with
        | FuncApp(expressions, range) ->
            match expressions with
            | RaiseWithTooManyArgs raiseType count ->
                Array.singleton
                    {
                        Range = range
                        Message = Resources.GetString ruleName
                        Fix = None
                        TypeChecks = List.Empty
                    }
            | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty
