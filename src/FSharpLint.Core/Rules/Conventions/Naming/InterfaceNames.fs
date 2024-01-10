module FSharpLint.Rules.InterfaceNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.TypeDefinition(SynTypeDefn(componentInfo, typeDef, _, _, _, _)) ->
        let isNotTypeExtension =
            match typeDef with
            | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.Augmentation(_), _, _) -> false
            | _ -> true

        if isNotTypeExtension then
            match componentInfo with
            | SynComponentInfo(attrs, _, _, identifier, _, _, _, _) ->
                match List.tryLast identifier with
                | Some typeIdentifier ->
                    if not (isMeasureType attrs) && isInterface typeDef then
                        Array.singleton (typeIdentifier, typeIdentifier.idText, None)
                    else
                        Array.empty
                | _ -> Array.empty
        else
            Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "InterfaceNames"
      Identifier = Identifiers.InterfaceNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule