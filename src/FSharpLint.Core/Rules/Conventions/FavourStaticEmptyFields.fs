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

    formatError errorMessageKey

let private generateError (fileContents: string) (range:FSharp.Compiler.Text.Range) (emptyLiteralType: EmptyLiteralType) =
    let suggestedFix = lazy(
        let replacementText =
            match emptyLiteralType with
            | EmptyStringLiteral -> "String.Empty"
            | EmptyListLiteral -> "List.Empty"
            | EmptyArrayLiteral -> "Array.empty"
        Some({ FromRange = range; ToText = replacementText }))
    Array.singleton
        { Range = range
          Message = getStaticEmptyErrorMessage range emptyLiteralType
          SuggestedFix = Some suggestedFix
          TypeChecks = List.Empty }

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.Const (SynConst.String ("", _, range), _)) -> 
        generateError args.FileContent range EmptyStringLiteral
    | AstNode.Expression(SynExpr.ArrayOrList (isArray, [], range)) ->
        let emptyLiteralType =
            if isArray then EmptyArrayLiteral else EmptyListLiteral
        generateError args.FileContent range emptyLiteralType
    | AstNode.Expression(SynExpr.Record(_, _, synExprRecordField, _)) ->
        let mapping =
            function
            | SynExprRecordField(_, _, expr, _) ->
                match expr with
                | Some(SynExpr.ArrayOrList(isArray, [], range)) ->
                    let emptyLiteralType = if isArray then EmptyArrayLiteral else EmptyListLiteral
                    generateError args.FileContent range emptyLiteralType
                | Some(SynExpr.Const (SynConst.String ("", _, range), _)) ->
                    generateError args.FileContent range EmptyStringLiteral
                | Some(SynExpr.App(_, _, _, SynExpr.Const (SynConst.String ("", _, range), _), _)) ->
                    generateError args.FileContent range EmptyStringLiteral
                | _ -> Array.empty
        synExprRecordField
        |> List.map mapping
        |> Array.concat
    | _ -> Array.empty


let rule =
    AstNodeRule
        {
            Name = "FavourStaticEmptyFields"
            Identifier = Identifiers.FavourStaticEmptyFields
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
