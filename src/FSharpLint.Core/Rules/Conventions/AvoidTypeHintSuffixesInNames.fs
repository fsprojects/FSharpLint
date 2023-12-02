module FSharpLint.Rules.AvoidTypeHintSuffixesInNames

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

let ruleName: string = "AvoidTypeHintSuffixesInNames"
let discouragedMemberSuffixes: List<string> = ["Lst"; "List"; "Array"; "Opt"; "Str"]

let private hasTypeHint (identifier: string) =
    let likelySuffixes =
        discouragedMemberSuffixes
        |> List.filter (fun text -> not (identifier.Equals text))
    likelySuffixes |> List.exists identifier.EndsWith

let private generateError (range: range) =
    { Range = range
      Message = Resources.GetString ruleName
      SuggestedFix = None
      TypeChecks = List.Empty }
    |> Array.singleton

let typeHintSuffixesInRecordFields (fields: List<SynField>) (range: range) =
    let rec traverse recordFields =
        match recordFields with
        | SynField(_, _, maybeVal, _, _, _, _, _)::rest ->
            match maybeVal with
            | Some field ->
                if hasTypeHint field.idText then
                    generateError range
                else
                    traverse rest
            | None ->
                traverse rest
        | [] -> Array.empty

    traverse fields

let typeHintSuffixesInUnionFields (fields: List<SynUnionCase>) (range: range) =
    let rec traverse unionCases =
        match unionCases with
        | SynUnionCase(_, ident, _, _, _, _)::rest ->
            if hasTypeHint ident.idText then
                generateError range
            else
                traverse rest
        | [] -> Array.empty

    traverse fields

let typeHintSuffixesInProperties (members: List<SynMemberDefn>) (range: range) =
    let rec traverse memberDefinitions =
        match memberDefinitions with
        | SynMemberDefn.AutoProperty(_, _, ident, _, _, _, _, _, _expression, _, _)::rest ->
            if hasTypeHint ident.idText then
                generateError range
            else
                traverse rest
        | SynMemberDefn.Member(SynBinding(_, _, _, _, _, _, _, pattern, _, _expression, _, _), _)::rest ->
            match pattern with
            | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
                if hasTypeHint ident.Tail.Head.idText then
                    generateError range
                else
                    traverse rest
            | _ -> traverse rest
        | _::rest -> traverse rest
        | [] -> Array.empty

    traverse members

let runner args =
    match args.AstNode with
    | TypeDefinition(SynTypeDefn(_, typeRepresentation, _members, _implicitCtor, range)) ->
        match typeRepresentation with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _) ->
            typeHintSuffixesInRecordFields fields range
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_, fields, _), _) ->
            typeHintSuffixesInUnionFields fields range
        | SynTypeDefnRepr.ObjectModel(_, members, _) ->
            typeHintSuffixesInProperties members range
        | _ -> Array.empty
    | _ -> Array.empty

let rule =
    { Name = ruleName
      Identifier = Identifiers.AvoidTypeHintSuffixesInNames
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
