module FSharpLint.Rules.ActivePatternNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getValueOrFunctionIdents _ pattern =
    match pattern with
    | SynPat.LongIdent(longIdent, _, _, _, _, _) ->
        match List.tryLast longIdent.LongIdent with
        | Some ident when isActivePattern ident ->
            Array.singleton ident
        | _ ->
            Array.empty
    | SynPat.Named(SynIdent(ident, _), _, _, _)
    | SynPat.OptionalVal(ident, _) ->
        if isActivePattern ident then
            Array.singleton ident
        else
            Array.empty
    | _ -> Array.empty

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.ForEach(_, _, _, true, pattern, _, _, _)) ->
        getPatternIdents AccessControlLevel.Private getValueOrFunctionIdents false pattern
    | AstNode.Binding(SynBinding(_, _, _, _, attributes, _, valData, pattern, _, _, _, _, _)) ->
        if not (isLiteral attributes) then
            match identifierTypeFromValData valData with
            | Value | Function ->
                let accessibility = getAccessControlLevel args.SyntaxArray args.NodeIndex
                getPatternIdents accessibility getValueOrFunctionIdents true pattern
            | _ -> Array.empty
        else
            Array.empty
    | _ -> Array.empty

let private getIdentifiersWithIdText =
    getIdentifiers
    >> Array.collect (fun identifier ->
        activePatternIdentifiers identifier
        |> Array.map (fun idText -> (identifier, idText, None)))

let rule config =
    { Name = "ActivePatternNames"
      Identifier = Identifiers.ActivePatternNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiersWithIdText } }
    |> toAstNodeRule
    |> AstNodeRule