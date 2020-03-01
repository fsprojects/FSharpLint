module FSharpLint.Rules.UnionCasesNames

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.UnionCase(SynUnionCase.UnionCase(_, identifier, _, _, _, _)) ->
        identifier |> Array.singleton
    | _ -> Array.empty

let rule config =
    { name = "UnionCasesNames"
      identifier = Identifiers.UnionCasesNames
      ruleConfig = { NamingRuleConfig.config = config; getIdentifiersToCheck = getIdentifiers >> addDefaults } }
    |> toAstNodeRule
    |> AstNodeRule