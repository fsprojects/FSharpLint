module FSharpLint.Rules.ExceptionNames

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.ExceptionRepresentation(SynExceptionDefnRepr.SynExceptionDefnRepr(_, unionCase, _, _, _, _)) ->
        match unionCase with
        | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
            (identifier, identifier.idText, None) |> Array.singleton
    | _ -> Array.empty

let rule config =
    { Name = "ExceptionNames"
      Identifier = Identifiers.ExceptionNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule