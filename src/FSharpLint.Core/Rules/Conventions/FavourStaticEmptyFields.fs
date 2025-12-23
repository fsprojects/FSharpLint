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
        Some({ FromRange = range; FromText = fileContents; ToText = replacementText }))
    Array.singleton
        { Range = range
          Message = getStaticEmptyErrorMessage range emptyLiteralType
          SuggestedFix = Some suggestedFix
          TypeChecks = List.Empty }

[<TailCall>]
let rec private processExpressions (errorsSoFar: array<WarningDetails>) (args: AstNodeRuleParams) (expressions: list<SynExpr>) =
    match expressions with
    | SynExpr.Const(SynConst.String ("", _, range), _) :: tail -> 
        let errors =
            Array.append
                errorsSoFar
                (generateError args.FileContent range EmptyStringLiteral)
        processExpressions errors args tail
    | SynExpr.ArrayOrList (isArray, [], range) :: tail ->
        let emptyLiteralType =
            if isArray then EmptyArrayLiteral else EmptyListLiteral
        let errors =
            Array.append
                errorsSoFar
                (generateError args.FileContent range emptyLiteralType)
        processExpressions errors args tail
    | SynExpr.App(_, _, _, argExpr, _) :: tail ->
        processExpressions errorsSoFar args (argExpr :: tail)
    | SynExpr.Record(_, _, synExprRecordFields, _) :: tail ->
        let mapping =
            function
            | SynExprRecordField(_, _, expr, _) -> expr
        let fieldExpressions = 
            synExprRecordFields
            |> List.choose mapping
        processExpressions errorsSoFar args (List.append fieldExpressions tail)
    | _ :: tail ->
        processExpressions errorsSoFar args tail
    | [] -> errorsSoFar

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(expr) -> 
        processExpressions Array.empty args (List.singleton expr)
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
