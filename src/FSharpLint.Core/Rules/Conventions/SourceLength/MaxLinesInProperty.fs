module FSharpLint.Rules.MaxLinesInProperty

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, valData, _, _, _, _, _, _) as binding) ->
        if identifierTypeFromValData valData = Property then
            Helper.SourceLength.checkSourceLengthRule config binding.RangeOfBindingWithRhs args.FileContent "Property" Array.empty
        else
            Array.empty
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "MaxLinesInProperty"
            Identifier = Identifiers.MaxLinesInProperty
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
