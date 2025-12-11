module FSharpLint.Rules.FavourStaticEmptyFields

open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

type private EmptyLiteralType =
    | EmptyStringLiteral
    | EmptyListLiteral
    | EmptyArrayLiteral

let private getStaticEmptyRuleViolationMessage  (range:FSharp.Compiler.Text.Range) (emptyLiteralType: EmptyLiteralType) =
    let messageKey =
        match emptyLiteralType with
        | EmptyStringLiteral -> "RulesFavourStaticEmptyFieldsForString"
        | EmptyListLiteral -> "RulesFavourStaticEmptyFieldsForList"
        | EmptyArrayLiteral -> "RulesFavourStaticEmptyFieldsForArray"

    Resources.GetString messageKey

let private generateViolation (fileContents: string) (range:FSharp.Compiler.Text.Range) (emptyLiteralType: EmptyLiteralType) =
    let autoFix = lazy(
        let replacementText =
            match emptyLiteralType with
            | EmptyStringLiteral -> "String.Empty"
            | EmptyListLiteral -> "List.Empty"
            | EmptyArrayLiteral -> "Array.empty"
        Some({ FromRange = range; FromText = fileContents; ToText = replacementText }))
    Array.singleton
        { Range = range
          Message = getStaticEmptyRuleViolationMessage range emptyLiteralType
          AutoFix = Some autoFix
          TypeChecks = List.Empty }

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.Const (SynConst.String ("", _, range), _)) -> 
        generateViolation args.FileContent range EmptyStringLiteral
    | AstNode.Expression(SynExpr.ArrayOrList (isArray, [], range)) ->
        let emptyLiteralType =
            if isArray then EmptyArrayLiteral else EmptyListLiteral
        generateViolation args.FileContent range emptyLiteralType
    | AstNode.Expression(SynExpr.Record(_, _, synExprRecordField, _)) ->
        let mapping =
            function
            | SynExprRecordField(_, _, expr, _) ->
                match expr with
                | Some(SynExpr.ArrayOrList(isArray, [], range)) ->
                    let emptyLiteralType = if isArray then EmptyArrayLiteral else EmptyListLiteral
                    generateViolation args.FileContent range emptyLiteralType
                | Some(SynExpr.Const (SynConst.String ("", _, range), _)) ->
                    generateViolation args.FileContent range EmptyStringLiteral
                | Some(SynExpr.App(_, _, _, SynExpr.Const (SynConst.String ("", _, range), _), _)) ->
                    generateViolation args.FileContent range EmptyStringLiteral
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
