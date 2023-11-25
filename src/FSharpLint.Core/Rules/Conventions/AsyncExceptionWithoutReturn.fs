module FSharpLint.Rules.AsyncExceptionWithoutReturn

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let checkAsyncExceptionWithoutReturn (args:AstNodeRuleParams) (range:Range) (doBangExpr:SynExpr) breadcrumbs =
    //match node with
    //    | SynExpr.App (_, _, SynExpr.Ident failwithId, expression, range) when
    //        failwithId.idText = "failwith"
    //        || failwithId.idText = "failwithf"
    //        ->
    //        match expression with
    //match node with 
    //    | SynExpr.App (_, _, _, _, SynExpr.CompExpr) when
                

let runner args =
    printf "\n________%A" args.AstNode
    match args.AstNode with
    | AstNode.Expression (SynExpr.DoBang (expr, range)) ->
        let parents = args.GetParents 5
        checkAsyncExceptionWithoutReturn args range expr parents
    | _ -> Array.empty

let rule =
    { Name = "AsyncExceptionWithoutReturn"
      Identifier = Identifiers.AsyncExceptionWithoutReturn
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
