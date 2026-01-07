module FSharpLint.Rules.TypeNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let rule config =
    let getIdentifiers (args:AstNodeRuleParams) =
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
                    | Some _ ->
                        if not (isMeasureType attrs) && not (isInterface typeDef) then
                            identifier
                            |> List.map (fun identifier -> (identifier, identifier.idText, None))
                            |> List.toArray
                        else
                            Array.empty
                    | _ -> Array.empty
            else
                Array.empty
        | _ -> Array.empty

    { Name = "TypeNames"
      Identifier = Identifiers.TypeNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule