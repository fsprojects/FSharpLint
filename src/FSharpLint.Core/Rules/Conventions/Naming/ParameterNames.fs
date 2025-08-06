module FSharpLint.Rules.ParameterNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getMemberIdents _ = function
    | SynPat.Named(SynIdent(ident, _), _, _, _)
    | SynPat.OptionalVal(ident, _) ->
        (ident, ident.idText, None) |> Array.singleton
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
        (ident, ident.idText, Some checkNotUnionCase) |> Array.singleton
    | SynPat.LongIdent(SynLongIdent([ident], _, _), _, _, SynArgPats.Pats([]), _, _) when not (isActivePattern ident) ->
        // Handle constructor parameters that are represented as LongIdent (e.g., PascalCase parameters)
        let checkNotUnionCase = checkNotUnionCase ident
        (ident, ident.idText, Some checkNotUnionCase) |> Array.singleton
    | _ -> Array.empty

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.MemberDefinition(memberDef) ->
        match memberDef with
        | SynMemberDefn.ImplicitCtor(_, _, ctorArgs, _, _, _, _) ->
            // ctorArgs is a SynPat, not SynSimplePats, so we need to handle it differently
            let accessControlLevel = getAccessControlLevel args.SyntaxArray args.NodeIndex
            getPatternIdents accessControlLevel (getValueOrFunctionIdents args.CheckInfo) true ctorArgs
        | _ -> Array.empty
    | AstNode.Binding(SynBinding(access, _, _, _, attributes, _, valData, pattern, _, _, _, _, _)) ->
        if not (isLiteral attributes) then
            match identifierTypeFromValData valData with
            | Function ->
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