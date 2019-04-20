module FSharpLint.Rules.EnumCasesNames

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.EnumCase(SynEnumCase.EnumCase(_, identifier, _, _, _)) ->
        identifier |> Array.singleton
    | _ -> Array.empty

let rule config =
    { name = "EnumCasesNames" 
      identifier = None
      ruleConfig = { NamingRuleConfig.config = config; getIdentifiersToCheck = getIdentifiers >> addDefaults } }
    |> toAstNodeRule
    |> AstNodeRule
