﻿module FSharpLint.Rules.GenericTypesNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.TypeDefinition(SynTypeDefn(componentInfo, _typeDef, _, _, _)) ->
        let checkTypes types =
            seq {
                for SynTyparDecl(_attr, synTypeDecl) in types do
                    match synTypeDecl with
                    | SynTypar(id, _, _) when not (isPascalCase id.idText) ->
                        yield (id, id.idText, None)
                    | _ -> ()
            }
            
        match componentInfo with
        | SynComponentInfo(_attrs, types, _, _identifier, _, _, _, _) ->
            checkTypes types |> Array.ofSeq
    | _ -> Array.empty

let rule config =
    { Name = "GenericTypesNames"
      Identifier = Identifiers.GenericTypesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule