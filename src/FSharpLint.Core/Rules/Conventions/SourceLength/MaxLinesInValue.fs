module FSharpLint.Rules.MaxLinesInValue

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding.Binding(_, _, _, _, _, _, valData, _, _, _, _, _) as binding) ->
       match identifierTypeFromValData valData with
       | Value -> Helper.SourceLength.checkSourceLengthRule config binding.RangeOfBindingAndRhs "Value"
       | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "MaxLinesInValue"
      Identifier = Identifiers.MaxLinesInValue
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule