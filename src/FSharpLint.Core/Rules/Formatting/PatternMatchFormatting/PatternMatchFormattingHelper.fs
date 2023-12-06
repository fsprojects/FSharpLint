module FSharpLint.Rules.Helper.PatternMatchFormatting

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let isActualPatternMatch (args:AstNodeRuleParams) rule =
    match args.AstNode with
    | AstNode.Expression (SynExpr.Match (_, _, clauses, range, _))
    | AstNode.Expression (SynExpr.MatchLambda (_, _, clauses, _, range))
    | AstNode.Expression (SynExpr.TryWith (_, clauses, range, _, _, _)) as node ->
        let isLambda =
            match node with
            | AstNode.Expression (SynExpr.MatchLambda _) -> true
            | _ -> false

        let isFunctionParameter =
            args.GetParents 3
            |> List.exists (function
                | Expression (SynExpr.Lambda _ ) -> true
                | _ -> false)

        // Ignore pattern matching in function parameters.
        if not (isFunctionParameter) then
            rule args range clauses isLambda
        else
            Array.empty
    | _ -> Array.empty