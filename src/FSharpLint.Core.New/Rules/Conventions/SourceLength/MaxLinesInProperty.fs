module FSharpLint.Rules.MaxLinesInProperty

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.Binding(SynBinding.Binding(_, _, _, _, _, _, valData, _, _, _, _, _) as binding) ->
       match identifierTypeFromValData valData with
       | Property -> Helper.SourceLength.checkSourceLengthRule config binding.RangeOfBindingAndRhs "Property"
       | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { name = "MaxLinesInProperty"
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner config; cleanup = ignore } }
    |> AstNodeRule