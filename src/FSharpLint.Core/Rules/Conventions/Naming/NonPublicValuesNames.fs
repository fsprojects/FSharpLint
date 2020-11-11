module FSharpLint.Rules.NonPublicValuesNames

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getValueOrFunctionIdents typeChecker isPublic pattern =
    let checkNotUnionCase ident =
        typeChecker |> Option.map (fun checker -> isNotUnionCase checker ident)

    match pattern with
    | SynPat.LongIdent(longIdent, _, _, _, _, _) ->
        // If a pattern identifier is made up of more than one part then it's not binding a new value.
        let singleIdentifier = List.length longIdent.Lid = 1

        match List.tryLast longIdent.Lid with
        | Some ident when not (isActivePattern ident) && singleIdentifier ->
            let checkNotUnionCase = checkNotUnionCase ident
            if not isPublic then
                (ident, ident.idText, checkNotUnionCase)
                |> Array.singleton
            else
                Array.empty
        | None | Some _ -> Array.empty
    | _ -> Array.empty

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.ForEach(_, _, true, pattern, _, _, _)) ->
        getPatternIdents false (getValueOrFunctionIdents args.CheckInfo) false pattern
    | AstNode.Binding(SynBinding.Binding(access, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
        if not (isLiteral attributes) then
            match identifierTypeFromValData valData with
            | Value | Function ->
                let isPublic = isPublic args.SyntaxArray args.NodeIndex
                getPatternIdents isPublic (getValueOrFunctionIdents args.CheckInfo) true pattern
            | _ -> Array.empty
        else
            Array.empty
    | AstNode.Expression(SynExpr.For(_, identifier, _, _, _, _, _)) ->
        (identifier, identifier.idText, None) |> Array.singleton
    | AstNode.Match(SynMatchClause.Clause(pattern, _, _, _, _)) ->
        match pattern with
        | SynPat.Named(_, identifier, isThis, _, _) when not isThis ->
            (identifier, identifier.idText, None) |> Array.singleton
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "NonPublicValuesNames"
      Identifier = Identifiers.NonPublicValuesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule
