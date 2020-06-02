module internal FSharpLint.Rules.Helper.TupleFormatting

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let isActualTuple (args:AstNodeRuleParams) rule =
    match args.AstNode with
    | AstNode.Expression (SynExpr.Tuple (_, tupleExprs, _, tupleRange)) ->
        let parentNode = args.GetParents 1 |> List.tryHead
        match parentNode with
        | Some (AstNode.Expression (SynExpr.App (funcExpr=(SynExpr.Ident ident)))) when ident.idText = "op_ColonColon" ->
            // cons operator is parsed as tuple, ignore it for tuple checking
            Array.empty
        | _ ->
            rule args tupleExprs tupleRange parentNode
    | _ ->
        Array.empty