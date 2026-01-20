module FSharpLint.Rules.MaxLinesInFunction

open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    let nodeAsFunctionBinding (node: AstNode) =
        match node with
        | AstNode.Binding(SynBinding(_, _, _, _, _, _, valData, _, _, _, _, _, _) as binding) ->
            match identifierTypeFromValData valData with
            | Function -> Some binding
            | _ -> None
        | _ -> None

    match nodeAsFunctionBinding args.AstNode with
    | Some binding ->
        let isInnerFunctionNode (node: AbstractSyntaxArray.Node) =
            match node.Actual with
            | AstNode.Binding(SynBinding(_) as currentBinding) ->
                let functionRange = binding.RangeOfBindingWithRhs
                let candidateRange = currentBinding.RangeOfBindingWithRhs
                if functionRange <> candidateRange
                    && ExpressionUtilities.rangeContainsOtherRange functionRange candidateRange then
                    nodeAsFunctionBinding node.Actual
                else
                    None
            | _ -> None

        let innerFunctions = 
            args.SyntaxArray
            |> Array.choose isInnerFunctionNode

        let innerFunctionRanges =
            innerFunctions
            |> Array.map (fun binding -> binding.RangeOfBindingWithRhs)

        Helper.SourceLength.checkSourceLengthRule config binding.RangeOfBindingWithRhs args.FileContent "Function" innerFunctionRanges
    | None ->
        Array.empty

let rule config =
    AstNodeRule
        {
            Name = "MaxLinesInFunction"
            Identifier = Identifiers.MaxLinesInFunction
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
