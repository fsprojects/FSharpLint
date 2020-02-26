module FSharpLint.Rules.ModuleNames

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, _, moduleKind, _, _, _, _, _) as synModule) ->
        if not (isImplicitModule synModule) && isModule moduleKind then
            identifier |> List.toArray
        else
            Array.empty
    | _ -> Array.empty

let rule config =
    { name = "ModuleNames"
      identifier = Identifiers.ModuleNames
      ruleConfig = { NamingRuleConfig.config = config; getIdentifiersToCheck = getIdentifiers >> addDefaults } }
    |> toAstNodeRule
    |> AstNodeRule

let newRule (config:NewNamingConfig) =
    rule
        { NamingConfig.naming = config.Naming
          underscores = config.Underscores
          prefix = config.Prefix
          suffix = config.Suffix }
