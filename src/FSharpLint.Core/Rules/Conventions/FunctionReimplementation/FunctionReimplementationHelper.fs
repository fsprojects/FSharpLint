module FSharpLint.Rules.Helper.FunctionReimplementation

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let rec getLambdaParamIdent = function
    | SynSimplePats.SimplePats([pattern], _, _) -> 
        let rec getIdent = function
            | SynSimplePat.Id(ident, _, _, _, _, _) -> ident
            | SynSimplePat.Typed(simplePattern, _, _)
            | SynSimplePat.Attrib(simplePattern, _, _) ->
                getIdent simplePattern

        getIdent pattern |> Some
    | SynSimplePats.SimplePats(_) -> None

let checkLambda (args:AstNodeRuleParams) checker =
    match args.AstNode with
    | AstNode.Expression(SynExpr.Lambda(_)) as lambdaExpression -> 
        match lambdaExpression with
        | Lambda(lambda, range) -> 
            if (not << List.isEmpty) lambda.Arguments then
                checker args.FileContent lambda range
            else Array.empty
        | _ -> Array.empty
    | _ -> Array.empty
