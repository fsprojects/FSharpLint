module FSharpLint.Rules.ExceptionNames

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.ExceptionRepresentation(SynExceptionDefnRepr.SynExceptionDefnRepr(_, unionCase, _, _, _, _)) ->
        match unionCase with
        | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
            identifier |> Array.singleton
    | _ -> Array.empty

let rule config =
    { name = "ExceptionNames" 
      identifier = None
      ruleConfig = { NamingRuleConfig.config = config; getIdentifiersToCheck = getIdentifiers >> addDefaults } }
    |> toAstNodeRule
    |> AstNodeRule
