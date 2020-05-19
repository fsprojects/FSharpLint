module FSharpLint.Rules.MaxLinesInMember

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding.Binding(_, _, _, _, _, _, valData, _, _, _, _, _) as binding) ->
        match identifierTypeFromValData valData with
        | Member -> Helper.SourceLength.checkSourceLengthRule config binding.RangeOfBindingAndRhs "Member"
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "MaxLinesInMember"
      Identifier = Identifiers.MaxLinesInMember
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule