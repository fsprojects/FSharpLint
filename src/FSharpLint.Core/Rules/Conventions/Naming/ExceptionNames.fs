module FSharpLint.Rules.ExceptionNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.ExceptionRepresentation(SynExceptionDefnRepr.SynExceptionDefnRepr(_, unionCase, _, _, _, _)) ->
        match unionCase with
        | SynUnionCase(_, SynIdent(identifier, _), _, _, _, _, _) ->
            Array.singleton (identifier, identifier.idText, None)
    | _ -> Array.empty

let rule config =
    { Name = "ExceptionNames"
      Identifier = Identifiers.ExceptionNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule