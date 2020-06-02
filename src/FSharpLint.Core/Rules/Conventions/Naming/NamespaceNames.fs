module internal FSharpLint.Rules.NamespaceNames

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, _, moduleKind, _, _, _, _, _) as synModule) ->
        if not (isImplicitModule synModule) && not (isModule moduleKind) then
            identifier
            |> List.map (fun identifier -> (identifier, identifier.idText, None))
            |> List.toArray
         else
             Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "NamespaceNames"
      Identifier = Identifiers.NamespaceNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule