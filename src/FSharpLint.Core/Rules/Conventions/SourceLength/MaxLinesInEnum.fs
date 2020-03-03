module FSharpLint.Rules.MaxLinesInEnum

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, repr, _, range)) ->
        match repr with
        | SynTypeDefnRepr.Simple(simpleRepr, _) ->
            match simpleRepr with
            | SynTypeDefnSimpleRepr.Enum(_) ->
                Helper.SourceLength.checkSourceLengthRule config range "Enum"
            | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "MaxLinesInEnum"
      Identifier = Identifiers.MaxLinesInEnum
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule