module FSharpLint.Rules.Helper.PatternMatchFormatting

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let isActualPatternMatch (args : AstNodeRuleParams) rule =
    match args.astNode with
    | AstNode.Expression (SynExpr.Match (_, _, clauses, range))
    | AstNode.Expression (SynExpr.MatchLambda (_, _, clauses, _, range))
    | AstNode.Expression (SynExpr.TryWith (_, _, clauses, range, _, _, _)) as node ->
        let isLambda =
            match node with
            | AstNode.Expression (SynExpr.MatchLambda _) -> true
            | _ -> false

        let isFunctionParameter =
            args.getParents 3 
            |> List.exists (function
                | Expression (SynExpr.Lambda _ ) -> true
                | _ -> false)

        // Ignore pattern matching in function parameters.
        if not (isFunctionParameter) then
            rule args range clauses isLambda
        else
            Array.empty
    | _ -> Array.empty