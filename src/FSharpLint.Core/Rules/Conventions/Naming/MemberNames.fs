module FSharpLint.Rules.MemberNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getMemberIdents _ =  function
    | SynPat.LongIdent(longIdent, _, _, _, _, _) ->
        match List.tryLast longIdent.LongIdent with
        | Some(ident) when ident.idText.StartsWith "op_" ->
            // Ignore members prefixed with op_, they are a special case used for operator overloading.
            Array.empty
        | None -> Array.empty
        | Some ident -> (ident, ident.idText, None) |> Array.singleton
    | _ -> Array.empty

let private isImplementingInterface parents =
    parents
    |> List.exists (function
        | AstNode.MemberDefinition (SynMemberDefn.Interface _) -> true
        | _ -> false)

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, attributes, _, valData, pattern, _, _, _, _, _)) ->
        let parents = args.GetParents 3
        if not (isLiteral attributes) && not (isImplementingInterface parents) then
            match identifierTypeFromValData valData with
            | Member | Property ->
                getPatternIdents AccessControlLevel.Private getMemberIdents true pattern
            | _ -> Array.empty
        else
            Array.empty
    | AstNode.MemberDefinition(memberDef) ->
        match memberDef with
        | SynMemberDefn.AbstractSlot(SynValSig(_, SynIdent(identifier, _), _, _, _, _, _, _, _, _, _, _), _, _, _) ->
            (identifier, identifier.idText, None) |> Array.singleton
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "MemberNames"
      Identifier = Identifiers.MemberNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule