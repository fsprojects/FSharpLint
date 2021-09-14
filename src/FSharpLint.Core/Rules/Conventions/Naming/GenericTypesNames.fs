module FSharpLint.Rules.GenericTypesNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.TypeDefinition(SynTypeDefn(componentInfo, typeDef, _, _, _)) ->
        match componentInfo with
        | SynComponentInfo(attrs, types, _, identifier, _, _, _, _) ->
            match List.tryLast types with
            | Some synType ->
                match synType with
                | SynTyparDecl(_attr, synTypeDecl) ->  
                    match synTypeDecl with
                    | SynTypar(id, _, _) when not (isPascalCase id.idText) ->
                        (id, id.idText, None) |> Array.singleton
                    | _ -> Array.empty
            | _ -> Array.empty       
    | _ -> Array.empty

let rule config =
    { Name = "GenericTypesNames"
      Identifier = Identifiers.GenericTypesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule