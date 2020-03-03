module FSharpLint.Rules.MaxLinesInRecord

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
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
    { Name = "MaxLinesInRecord"
      Identifier = Identifiers.MaxLinesInRecord
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule