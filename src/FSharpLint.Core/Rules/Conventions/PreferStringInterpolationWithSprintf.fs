module FSharpLint.Rules.PreferStringInterpolationWithSprintf

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let mutable moduleIdentifiers = Set.empty
let mutable letIdentifiers = Set.empty

let private isStringFormat (identifiers: List<Ident>) =
    "String" = identifiers.[0].idText && "Format" = identifiers.[1].idText

let private genereateErrorMessage range =
    { Range = range
      Message = Resources.GetString "RulesPreferStringInterpolationWithSprintf"
      SuggestedFix = None
      TypeChecks = List.empty }
    |> Array.singleton

let runner args =
    match args.AstNode with
    | AstNode.Expression(SynExpr.App(_, _, SynExpr.LongIdent(_, LongIdentWithDots(ids, _), _, _), paren, range)) when ids.Length = 2 && isStringFormat ids ->
        let isTopMember (text: string) =
            moduleIdentifiers.Contains text
        match paren with
        | SynExpr.Paren(SynExpr.Tuple(_, [SynExpr.Const(SynConst.String(_, _, _), _); _], _, _), _, _, _) ->
            genereateErrorMessage range
        | SynExpr.Paren(SynExpr.Tuple(_, [SynExpr.Ident identifier; _], _, _), _, _, range)  ->

            if isTopMember identifier.idText then
                genereateErrorMessage range
            else
                let isNamedBinding binding =
                    match binding with
                    | SynBinding(_, _, _, _, _, _, _, SynPat.Named(_, ident, _, _, _), _, _, _, _) ->
                        identifier.idText = ident.idText
                    | _ -> false

                let isVisible asts =
                    let rec loop asts =
                        match asts with
                        | AstNode.Expression (SynExpr.LetOrUse (_, _, [binding], _, _)) :: rest ->
                            isNamedBinding binding || loop rest
                        | _ :: rest -> loop rest
                        | [] -> false
                    loop asts

                let getTopLevelParent index =
                    let rec loop index =
                        let parents = List.rev (args.GetParents index)
                        match parents with
                        | AstNode.ModuleDeclaration (SynModuleDecl.Let _) :: rest -> rest
                        | _ -> loop (index + 1)
                    loop index

                if letIdentifiers.Contains identifier.idText && isVisible (getTopLevelParent 2) then
                    genereateErrorMessage range
                else
                    Array.empty
        | _ -> Array.empty
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, SynPat.Named(_, identifier, _, _, _), _, SynExpr.Const(SynConst.String(value, _, _), _), range, _)) when value.Contains "{0}" ->
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
