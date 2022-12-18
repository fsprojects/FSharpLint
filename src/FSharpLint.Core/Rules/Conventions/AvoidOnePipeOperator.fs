module FSharpLint.Rules.AvoidOnePipeOperator

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    printfn "%A" args.AstNode
    let error = Array.empty
      
    error

let rule =
    { Name = "AvoidOnePipeOperator"
      Identifier = Identifiers.AvoidOnePipeOperator
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule