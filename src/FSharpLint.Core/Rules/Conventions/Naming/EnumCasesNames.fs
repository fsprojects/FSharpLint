module FSharpLint.Rules.EnumCasesNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.EnumCase(SynEnumCase(_, identifier, _, _, _, _)) ->
        (identifier, identifier.idText, None) |> Array.singleton
    | _ -> Array.empty

let rule config =
    { Name = "EnumCasesNames"
      Identifier = Identifiers.EnumCasesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule