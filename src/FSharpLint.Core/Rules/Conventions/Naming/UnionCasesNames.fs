module FSharpLint.Rules.UnionCasesNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let rule config =
    let getIdentifiers (args:AstNodeRuleParams) =
        match args.AstNode with
        | AstNode.UnionCase(SynUnionCase(_, SynIdent(identifier, _), _, _, _, _, _)) ->
            Array.singleton (identifier, identifier.idText, None)
        | _ -> Array.empty

    { Name = "UnionCasesNames"
      Identifier = Identifiers.UnionCasesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule