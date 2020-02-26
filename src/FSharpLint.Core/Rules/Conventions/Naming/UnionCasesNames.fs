module FSharpLint.Rules.UnionCasesNames

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
        identifier |> Array.singleton
    | _ -> Array.empty

let rule config =
    { Name = "UnionCasesNames"
      Identifier = Identifiers.UnionCasesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers >> addDefaults } }
    |> toAstNodeRule
    |> AstNodeRule