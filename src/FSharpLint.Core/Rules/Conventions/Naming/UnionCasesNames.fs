module FSharpLint.Rules.UnionCasesNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.UnionCase(SynUnionCase(_, SynIdent(identifier, _), _, _, _, _, _)) ->
        Array.singleton (identifier, identifier.idText, None)
    | _ -> Array.empty

let rule config =
    { Name = "UnionCasesNames"
      Identifier = Identifiers.UnionCasesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule