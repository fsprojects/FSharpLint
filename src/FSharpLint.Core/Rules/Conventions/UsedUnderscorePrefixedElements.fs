module FSharpLint.Rules.UsedUnderscorePrefixedElements

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules



let runner (args:AstNodeRuleParams) =
    Array.empty

let rule =
    { Name = "UsedUnderscorePrefixedElements"
      Identifier = Identifiers.UsedUnderscorePrefixedElements
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule