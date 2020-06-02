module internal FSharpLint.Rules.MaxLinesInProperty

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding.Binding(_, _, _, _, _, _, valData, _, _, _, _, _) as binding) ->
       match identifierTypeFromValData valData with
       | Property -> Helper.SourceLength.checkSourceLengthRule config binding.RangeOfBindingAndRhs "Property"
       | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "MaxLinesInProperty"
      Identifier = Identifiers.MaxLinesInProperty
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule