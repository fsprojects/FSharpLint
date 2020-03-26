module FSharpLint.Rules.EnumCasesNames

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
        identifier |> Array.singleton
    | _ -> Array.empty

let rule config =
    { Name = "EnumCasesNames"
      Identifier = Identifiers.EnumCasesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers >> addDefaults } }
    |> toAstNodeRule
    |> AstNodeRule