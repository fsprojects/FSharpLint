module FSharpLint.Rules.MaxLinesInRecord

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, repr, _, range)) ->
        match repr with
        | SynTypeDefnRepr.Simple(simpleRepr, _) ->
            match simpleRepr with
            | SynTypeDefnSimpleRepr.Record(_) ->
                Helper.SourceLength.checkSourceLengthRule config range "Record"
            | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { name = "MaxLinesInRecord"
      identifier = Identifiers.MaxLinesInRecord
      ruleConfig = { AstNodeRuleConfig.runner = runner config; cleanup = ignore } }
    |> AstNodeRule

let newRule (config:Helper.SourceLength.NewConfig) =
    rule { Helper.SourceLength.Config.maxLines = config.MaxLines }