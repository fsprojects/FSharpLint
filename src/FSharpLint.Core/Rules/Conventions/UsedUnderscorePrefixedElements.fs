module FSharpLint.Rules.UsedUnderscorePrefixedElements

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules



let runner (args:AstNodeRuleParams) =
    printfn "AstNode: %A" args.AstNode
    let error =
      match args.AstNode with
      | AstNode.Expression (SynExpr.LetOrUse (isRecursive, isUse, bindings, body, range)) ->
           match bindings with
           | head ->
                ()
           | _ ->
               ()
          let parents = args.GetParents 5
          printfn "%A" parents
          Array.empty
          // checkRecursiveAsyncFunction args range expr parents
      | _ -> Array.empty

    printfn "----------------"

    error

let rule =
    { Name = "UsedUnderscorePrefixedElements"
      Identifier = Identifiers.UsedUnderscorePrefixedElements
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule