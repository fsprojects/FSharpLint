module FSharpLint.Rules.MaxLinesInValue

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, valData, _, _, _, _, _, _) as binding) ->
        if identifierTypeFromValData valData = Value then
            Helper.SourceLength.checkSourceLengthRule config binding.RangeOfBindingWithRhs args.FileContent "Value" Array.empty
        else
            Array.empty
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "MaxLinesInValue"
            Identifier = Identifiers.MaxLinesInValue
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
