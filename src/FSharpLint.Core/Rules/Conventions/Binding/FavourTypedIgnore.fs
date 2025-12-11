module FSharpLint.Rules.FavourTypedIgnore

open System
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private runner (args: AstNodeRuleParams) =
    let generateViolation identifier range text =

        let autoFix =
            lazy
                (ExpressionUtilities.tryFindTextOfRange range text
                 |> Option.map
                     (fun fromText ->
                         { FromText = fromText
                           FromRange = range
                           ToText = identifier }))

        {
            Range = range
            Message = String.Format(Resources.GetString "RulesFavourTypedIgnore", identifier)
            AutoFix = Some autoFix
            TypeChecks = List.Empty
        }

    let isTyped expression identifier range text =
        match expression with
        | SynExpr.Typed (_id, _synType, _range) -> Array.empty
        | _ ->
            generateViolation identifier range text
            |> Array.singleton

    let ignoreFunc = "ignore"

    match args.AstNode with
    | AstNode.Expression (SynExpr.App (_, _, expression, SynExpr.Ident identifier, range)) when
        identifier.idText = ignoreFunc
        ->
        isTyped expression identifier.idText range identifier.idText
    | AstNode.Expression (SynExpr.App (_, _, SynExpr.Ident identifier, expression, range)) when
        identifier.idText = ignoreFunc
        ->
        match expression with
        | SynExpr.Paren (expr, _, _, _) -> isTyped expr identifier.idText range identifier.idText
        | _ ->
            generateViolation identifier.idText range identifier.idText
            |> Array.singleton
    | _ -> Array.empty

/// Checks if any code uses untyped ignore
let rule =
    AstNodeRule
        {
            Name = "FavourTypedIgnore"
            Identifier = Identifiers.FavourTypedIgnore
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
