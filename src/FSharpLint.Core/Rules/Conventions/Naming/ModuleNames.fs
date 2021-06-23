module FSharpLint.Rules.ModuleNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, _, moduleKind, _, _, _, _, _) as synModule) ->
        if not (isImplicitModule synModule) && isModule moduleKind then
            identifier
            |> List.map (fun identifier -> (identifier, identifier.idText, None))
            |> List.toArray
        else
            Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "ModuleNames"
      Identifier = Identifiers.ModuleNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule