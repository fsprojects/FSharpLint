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

let private getStaticEmptyErrorMessage  (range:FSharp.Compiler.Text.Range) (emptyLiteralType: EmptyLiteralType) =
    let errorMessageKey =
        match emptyLiteralType with
        | EmptyStringLiteral -> "RulesFavourStaticEmptyFieldsForString"
        | EmptyListLiteral -> "RulesFavourStaticEmptyFieldsForList"
        | EmptyArrayLiteral -> "RulesFavourStaticEmptyFieldsForArray"

    let formatError errorName =
        Resources.GetString errorName

    errorMessageKey |> formatError

let private generateError (range:FSharp.Compiler.Text.Range) (emptyLiteralType: EmptyLiteralType) =
    { Range = range
      Message = getStaticEmptyErrorMessage range emptyLiteralType
      SuggestedFix = None
      TypeChecks = List.Empty }
    |> Array.singleton

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.Const (SynConst.String ("", _, range), _)) -> 
        generateError range EmptyStringLiteral
    | AstNode.Expression(SynExpr.ArrayOrList (isArray, [], range)) ->
        let emptyLiteralType =
            if isArray then EmptyArrayLiteral else EmptyListLiteral
        generateError range emptyLiteralType
    | AstNode.Expression(SynExpr.Record(_, _, synExprRecordField, _)) ->
        synExprRecordField
        |> List.map (fun field ->
            match field with
            | SynExprRecordField(_, _, expr, _) ->
                match expr with
                | Some(SynExpr.ArrayOrList(isArray, [], range)) ->
                    let emptyLiteralType = if isArray then EmptyArrayLiteral else EmptyListLiteral
                    generateError range emptyLiteralType
                | Some(SynExpr.Const (SynConst.String ("", _, range), _)) ->
                    generateError range EmptyStringLiteral
                | _ -> Array.empty)
        |> Array.concat
    | _ -> Array.empty


let rule =
    { Name = "FavourStaticEmptyFields"
      Identifier = Identifiers.FavourStaticEmptyFields
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
