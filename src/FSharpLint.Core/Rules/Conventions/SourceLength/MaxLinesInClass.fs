module FSharpLint.Rules.MaxLinesInClass

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, repr, _, range)) ->
        match repr with
        | SynTypeDefnRepr.ObjectModel(_) ->
            Helper.SourceLength.checkSourceLengthRule config range "Classes and interface"
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { name = "MaxLinesInClass"
      identifier = Identifiers.MaxLinesInClass
      ruleConfig = { AstNodeRuleConfig.runner = runner config; cleanup = ignore } }
    |> AstNodeRule

let newRule (config:Helper.SourceLength.NewConfig) =
    rule { Helper.SourceLength.Config.maxLines = config.MaxLines }