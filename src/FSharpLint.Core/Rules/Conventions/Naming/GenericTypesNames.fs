module FSharpLint.Rules.GenericTypesNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.TypeDefinition(SynTypeDefn(componentInfo, _typeDef, _, _, _, _)) ->
        let checkTypes types =
            seq {
                for SynTyparDecl(_attr, SynTypar(id, _, _), _, _) in types do
                    yield (id, id.idText, None)
            }

        match componentInfo with
        | SynComponentInfo(_attrs, maybeTypes, _, _identifier, _, _, _, _) ->
            match maybeTypes with
            | Some types -> checkTypes types.TyparDecls |> Array.ofSeq
            | None -> Array.empty
    | AstNode.Type(SynType.Var(SynTypar(id, _, _), _)) ->
        (id, id.idText, None) |> Array.singleton
    | _ -> Array.empty

let rule config =
    { Name = "GenericTypesNames"
      Identifier = Identifiers.GenericTypesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule