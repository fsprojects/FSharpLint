module FSharpLint.Rules.EnsureTailCallDiagnosticsInRecursiveFunctions

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    failwith "Not yet implemented"

let rule =
    { Name = "EnsureTailCallDiagnosticsInRecursiveFunctions"
      Identifier = Identifiers.EnsureTailCallDiagnosticsInRecursiveFunctions
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
