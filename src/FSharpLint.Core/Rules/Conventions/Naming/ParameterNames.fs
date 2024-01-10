module FSharpLint.Rules.ParameterNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getMemberIdents _ = function
    | SynPat.Named(SynIdent(ident, _), _, _, _)
    | SynPat.OptionalVal(ident, _) ->
        Array.singleton (ident, ident.idText, None)
    | _ -> Array.empty

let private getValueOrFunctionIdents typeChecker _accessibility pattern =
    let checkNotUnionCase ident = fun () ->
        typeChecker
        |> Option.map (fun checker -> isNotUnionCase checker ident)
        |> Option.defaultValue true

    match pattern with
    | SynPat.Named(SynIdent(ident, _), _, _, _)
    | SynPat.OptionalVal(ident, _) when not (isActivePattern ident) ->
        let checkNotUnionCase = checkNotUnionCase ident
        Array.singleton (ident, ident.idText, Some checkNotUnionCase)
    | _ -> Array.empty

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.MemberDefinition(memberDef) ->
        match memberDef with
        | SynMemberDefn.ImplicitCtor(_, _, ctorArgs, _, _, _) ->
            ctorArgs
            |> extractPatterns
            |> List.toArray
            |> Array.choose identFromSimplePat
            |> Array.map (fun ident -> (ident, ident.idText, None))
        | _ -> Array.empty
    | AstNode.Binding(SynBinding(access, _, _, _, attributes, _, valData, pattern, _, _, _, _, _)) ->
        if not (isLiteral attributes) then
            match identifierTypeFromValData valData with
            | Value | Function ->
                let accessControlLevel = getAccessControlLevel args.SyntaxArray args.NodeIndex
                getPatternIdents accessControlLevel (getValueOrFunctionIdents args.CheckInfo) true pattern
            | Member | Property ->
                getPatternIdents AccessControlLevel.Private getMemberIdents true pattern
            | _ -> Array.empty
        else
            Array.empty
    | AstNode.Expression(SynExpr.ForEach(_, _, _, true, pattern, _, _, _)) ->
        getPatternIdents AccessControlLevel.Private (getValueOrFunctionIdents args.CheckInfo) false pattern
    | _ -> Array.empty

let rule config =
    { Name = "ParameterNames"
      Identifier = Identifiers.ParameterNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule