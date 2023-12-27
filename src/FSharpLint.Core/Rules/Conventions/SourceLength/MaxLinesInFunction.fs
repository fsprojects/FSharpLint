module FSharpLint.Rules.MaxLinesInFunction

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, valData, _, _, _, _, _) as binding) ->
        match identifierTypeFromValData valData with
        | Function -> Helper.SourceLength.checkSourceLengthRule config binding.RangeOfBindingWithRhs args.FileContent "Function"
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "MaxLinesInFunction"
      Identifier = Identifiers.MaxLinesInFunction
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule