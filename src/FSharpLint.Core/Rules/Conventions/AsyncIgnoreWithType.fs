module FSharpLint.Rules.AsyncIgnoreWithType

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules


let runner (args:AstNodeRuleParams) =
    Array.empty

let rule =
    { Name = "AsyncIgnoreWithType"
      Identifier = Identifiers.AvoidTooShortNames
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
