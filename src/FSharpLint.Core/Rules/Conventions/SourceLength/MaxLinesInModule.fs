module FSharpLint.Rules.MaxLinesInModule

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_, _, (NamedModule | AnonModule), _, _, _, _, range)) ->
        Helper.SourceLength.checkSourceLengthRule config range "Module"
    | _ -> Array.empty

let rule config =
    { name = "MaxLinesInModule"
      identifier = Identifiers.MaxLinesInModule
      ruleConfig = { AstNodeRuleConfig.runner = runner config; cleanup = ignore } }
    |> AstNodeRule