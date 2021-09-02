module FSharpLint.Rules.ParameterNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getMemberIdents _ = function
    | SynPat.Named(_, ident, _, _, _)
    | SynPat.OptionalVal(ident, _) ->
        (ident, ident.idText, None) |> Array.singleton
    | _ -> Array.empty

let private getValueOrFunctionIdents typeChecker _accessibility pattern =
    let checkNotUnionCase ident = fun () ->
        typeChecker
        |> Option.map (fun checker -> isNotUnionCase checker ident)
        |> Option.defaultValue true

    match pattern with
    | SynPat.Named(_, ident, _, _, _)
    | SynPat.OptionalVal(ident, _) when not (isActivePattern ident) ->
        let checkNotUnionCase = checkNotUnionCase ident
        (ident, ident.idText, Some checkNotUnionCase) |> Array.singleton
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
    | AstNode.Binding(SynBinding(access, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
        if not (isLiteral attributes) then
            match identifierTypeFromValData valData with
            | Value | Function ->
                let accessibility = getAccessibility args.SyntaxArray args.NodeIndex
                getPatternIdents accessibility (getValueOrFunctionIdents args.CheckInfo) true pattern
            | Member | Property ->
                getPatternIdents Accessibility.Private getMemberIdents true pattern
            | _ -> Array.empty
        else
            Array.empty
    | AstNode.Expression(SynExpr.ForEach(_, _, true, pattern, _, _, _)) ->
        getPatternIdents Accessibility.Private (getValueOrFunctionIdents args.CheckInfo) false pattern
    | _ -> Array.empty

let rule config =
    { Name = "ParameterNames"
      Identifier = Identifiers.ParameterNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule