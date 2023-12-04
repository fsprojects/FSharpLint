module FSharpLint.Rules.SuggestUseAutoProperty

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private runner (args: AstNodeRuleParams) =
    failwith "Not yet implemented"

let rule =
    { Name = "SuggestUseAutoProperty"
      Identifier = Identifiers.SuggestUseAutoProperty
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
