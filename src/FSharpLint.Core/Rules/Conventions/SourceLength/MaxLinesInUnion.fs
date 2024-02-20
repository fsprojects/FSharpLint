module FSharpLint.Rules.MaxLinesInUnion

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.TypeDefinition(SynTypeDefn(_, repr, _, _, range, _)) ->
        match repr with
        | SynTypeDefnRepr.Simple(simpleRepr, _) ->
            match simpleRepr with
            | SynTypeDefnSimpleRepr.Union(_) ->
                Helper.SourceLength.checkSourceLengthRule config range args.FileContent "Union"
            | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "MaxLinesInUnion"
            Identifier = Identifiers.MaxLinesInUnion
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
