module FSharpLint.Rules.NamespaceNames

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, _, moduleKind, _, _, _, _, _) as synModule) ->
        if not (isImplicitModule synModule) && not (isModule moduleKind) then
            identifier |> List.toArray
         else
             Array.empty
    | _ -> Array.empty

let rule config =
    { name = "NamespaceNames" 
      identifier = Identifiers.NamespaceNames
      ruleConfig = { NamingRuleConfig.config = config; getIdentifiersToCheck = getIdentifiers >> addDefaults } }
    |> toAstNodeRule
    |> AstNodeRule
