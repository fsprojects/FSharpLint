module FSharpLint.Rules.FavourStaticEmptyFields

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

type private EmptyLiteralType =
    | EmptyStringLiteral
    | EmptyListLiteral
    | EmptyArrayLiteral

let private getEmptyLiteralType (str: string): EmptyLiteralType =
    if str.Length = 0 then
        EmptyLiteralType.EmptyStringLiteral
    elif str = "[]" then
        EmptyLiteralType.EmptyListLiteral
    else
        EmptyLiteralType.EmptyArrayLiteral

let private getStaticEmptyErrorMessage  (range:FSharp.Compiler.Text.Range) (emptyLiteralType: EmptyLiteralType) =
    let errorMessageKey =
        match emptyLiteralType with
        | EmptyStringLiteral -> "RulesFavourStaticEmptyFieldsForString"
        | EmptyListLiteral -> "RulesFavourStaticEmptyFieldsForList"
        | EmptyArrayLiteral -> "RulesFavourStaticEmptyFieldsForArray"

    let formatError errorName =
        Resources.GetString errorName

    errorMessageKey |> formatError |> Array.singleton

let private generateError (range:FSharp.Compiler.Text.Range) (idText:string) (emptyLiteralType: EmptyLiteralType) =
    if idText = "" || idText = "[]" then
        getStaticEmptyErrorMessage range emptyLiteralType
        |> Array.map (fun message ->
            { Range = range
              Message = message
              SuggestedFix = None
              TypeChecks = List.Empty })
    else
        Array.empty

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression expr ->
        match expr with
        | SynExpr.App (_, _, SynExpr.Ident failwithId, expression, range) ->
            match expression with
            | SynExpr.Const (SynConst.String (id, _, _), _) when id = "" ->
                (range, id, None, getEmptyLiteralType id)
                |> Array.singleton
                |> Array.collect (fun (range, idText, typeCheck, emptyLiteralType) ->
                    let suggestions = generateError range idText emptyLiteralType
                    suggestions |> Array.map (fun suggestion -> { suggestion with TypeChecks = Option.toList typeCheck }))
            | _ -> Array.empty
        | SynExpr.ArrayOrList (_, id, range) when "[]" = sprintf "%A" id || "[||]" = sprintf "%A" id ->
            (range, sprintf "%A" id, None, getEmptyLiteralType (sprintf "%A" id))
            |> Array.singleton
            |> Array.collect (fun (range, idText, typeCheck, emptyLiteralType) ->
                let suggestions = generateError range idText emptyLiteralType
                suggestions |> Array.map (fun suggestion -> { suggestion with TypeChecks = Option.toList typeCheck }))
        | SynExpr.Const (SynConst.String (id, _, range), _) when id = "" ->
            (range, id, None, getEmptyLiteralType id)
            |> Array.singleton
            |> Array.collect (fun (range, idText, typeCheck, emptyLiteralType) ->
                let suggestions = generateError range idText emptyLiteralType
                suggestions |> Array.map (fun suggestion -> { suggestion with TypeChecks = Option.toList typeCheck }))
        | _ -> Array.empty
    | Binding(SynBinding(_, _, _, _, _, _, _, _, _, expression, _, _)) ->
        match expression with
        | SynExpr.Const (SynConst.String (id, _, range), _) when id = "" ->
            (range, id, None, getEmptyLiteralType id)
            |> Array.singleton
            |> Array.collect (fun (range, idText, typeCheck, emptyLiteralType) ->
                let suggestions = generateError range idText emptyLiteralType
                suggestions |> Array.map (fun suggestion -> { suggestion with TypeChecks = Option.toList typeCheck }))
        | SynExpr.ArrayOrList (_, id, range) when "[]" = sprintf "%A" id || "[||]" = sprintf "%A" id ->
            (range, sprintf "%A" id, None, getEmptyLiteralType (sprintf "%A" id))
            |> Array.singleton
            |> Array.collect (fun (range, idText, typeCheck, emptyLiteralType) ->
                let suggestions = generateError range idText emptyLiteralType
                suggestions |> Array.map (fun suggestion -> { suggestion with TypeChecks = Option.toList typeCheck }))
        | _ -> Array.empty
    | _ -> Array.empty


let rule =
    { Name = "FavourStaticEmptyFields"
      Identifier = Identifiers.FavourStaticEmptyFields
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
