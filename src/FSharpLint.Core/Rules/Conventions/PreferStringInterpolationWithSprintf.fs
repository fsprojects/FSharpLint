module FSharpLint.Rules.PreferStringInterpolationWithSprintf

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let mutable moduleIdentifiers = Set.empty
let mutable letIdentifiers = Set.empty

[<TailCall>]
let rec isVisible (id: Ident) asts =
    let isNamedBinding binding =
        match binding with
        | SynBinding(_, _, _, _, _, _, _, SynPat.Named(SynIdent.SynIdent(ident, _), _, _, _), _, _, _, _, _) ->
            id.idText = ident.idText
        | _ -> false

    match asts with
    | AstNode.Expression (SynExpr.LetOrUse (_, _, [binding], _, _, _)) :: rest ->
        isNamedBinding binding || isVisible id rest
    | _ :: rest -> isVisible id rest
    | [] -> false

[<TailCall>]
let rec getTopLevelParent args index =
    let parents = List.rev (args.GetParents index)
    match parents with
    | AstNode.ModuleDeclaration (SynModuleDecl.Let _) :: rest -> rest
    | _ -> getTopLevelParent args (index + 1)

let runner args =
    let isStringFormat (identifiers: List<Ident>) =
        "String" = identifiers.[0].idText && "Format" = identifiers.[1].idText

    let emitViolation range =
        { Range = range
          Message = Resources.GetString "RulesPreferStringInterpolationWithSprintf"
          SuggestedFix = None
          TypeChecks = List.empty }
        |> Array.singleton

    match args.AstNode with
    | AstNode.Expression(SynExpr.App(_, _, SynExpr.LongIdent(_, SynLongIdent(ids, _, _), _, _), paren, range)) when ids.Length = 2 && isStringFormat ids ->
        let isTopMember (text: string) =
            moduleIdentifiers.Contains text
        match paren with
        | SynExpr.Paren(SynExpr.Tuple(_, [SynExpr.Const(SynConst.String(_), _); _], _, _), _, _, _) ->
            emitViolation range
        | SynExpr.Paren(SynExpr.Tuple(_, [SynExpr.Ident identifier; _], _, _), _, _, range)  ->

            if isTopMember identifier.idText then
                emitViolation range
            else
                if letIdentifiers.Contains identifier.idText && isVisible identifier (getTopLevelParent args 2) then
                    emitViolation range
                else
                    Array.empty
        | _ -> Array.empty
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, SynPat.Named(SynIdent.SynIdent(identifier, _), _, _, _), _, SynExpr.Const(SynConst.String(value, _, _), _), _, _, _)) when value.Contains "{0}" ->
        let parents = args.GetParents 1
        match parents with
        | AstNode.ModuleDeclaration(SynModuleDecl.Let _) :: _ ->
            moduleIdentifiers <- moduleIdentifiers.Add(identifier.idText)
        | _ -> letIdentifiers <- letIdentifiers.Add(identifier.idText)
        Array.empty
    | AstNode.ModuleDeclaration(SynModuleDecl.Let _) ->
        letIdentifiers <- Set.empty
        Array.empty
    | AstNode.ModuleDeclaration(SynModuleDecl.NestedModule _) ->
        moduleIdentifiers <- Set.empty
        letIdentifiers <- Set.empty
        Array.empty
    | _ -> Array.empty

let cleanup () =
    moduleIdentifiers <- Set.empty
    letIdentifiers <- Set.empty

let rule =
    { Name = "PreferStringInterpolationWithSprintf"
      Identifier = Identifiers.PreferStringInterpolationWithSprintf
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = cleanup } }
    |> AstNodeRule
