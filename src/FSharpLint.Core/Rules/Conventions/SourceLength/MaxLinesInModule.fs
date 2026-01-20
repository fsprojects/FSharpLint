module FSharpLint.Rules.MaxLinesInModule

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_, _, (SynModuleOrNamespaceKind.NamedModule | SynModuleOrNamespaceKind.AnonModule ), _, _, _, _, range, _)) ->
        Helper.SourceLength.checkSourceLengthRule config range args.FileContent "Module" Array.empty
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "MaxLinesInModule"
            Identifier = Identifiers.MaxLinesInModule
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
