module FSharpLint.Rules.FavourNonMutablePropertyInitialization

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities
open System

[<TailCall>]
let rec private extraFromBindings (bindings: List<SynBinding>) (classInstances: List<string>) =
    match bindings with
    | SynBinding(_, _, _, _, _, _, _, SynPat.Named(SynIdent(ident, _), _, _, _), _, _expression, _, _, _)::rest ->
        extraFromBindings rest (ident.idText::classInstances)
    | _ -> classInstances

[<TailCall>]
let rec private processLetBinding (instanceNames: Set<string>) (body: SynExpr) (continuation: unit -> array<WarningDetails>) : array<WarningDetails> =
    let getWarningDetails (ident: Ident) =
        let formatError errorName =
            String.Format(Resources.GetString errorName, ident.idText)

        "RulesFavourNonMutablePropertyInitializationError"
        |> formatError
        |> Array.singleton
        |> Array.map (fun message ->
            { Range = ident.idRange
              Message = message
              SuggestedFix = None
              TypeChecks = List.Empty })
    
    let extraInstanceMethod (app:SynExpr) (instanceMethodCalls: List<string>) =
        match app with
        | SynExpr.App(_, _, expression, _, _) ->
            match expression with
            | SynExpr.LongIdent(_, SynLongIdent(identifiers, _, _), _, _) ->
                match List.tryLast identifiers with
                | Some _ ->
                    identifiers.[0].idText::instanceMethodCalls
                | _ -> instanceMethodCalls
            | _ -> instanceMethodCalls
        | _ -> instanceMethodCalls

    Array.append
        (match body with
        | SynExpr.LongIdentSet(SynLongIdent(identifiers, _, _), _, _) ->
            match identifiers with
            | [instanceIdent; propertyIdent] when Set.contains instanceIdent.idText instanceNames ->
                getWarningDetails propertyIdent
            | _ -> Array.empty
        | SynExpr.Sequential(_, _, expr1, expr2, _, _) ->
            let instanceNames =
                Set.difference
                    instanceNames
                    (extraInstanceMethod expr1 List.empty |> Set.ofList)
            processLetBinding
                instanceNames
                expr1
                (fun () -> processLetBinding instanceNames expr2 returnEmptyArray)
        | _ -> Array.empty)
        (continuation())

and [<TailCall>] processExpression (expression: SynExpr) (continuation: unit -> array<WarningDetails>) : array<WarningDetails> =
    Array.append
        (match expression with
        | SynExpr.LetOrUse(_, _, bindings, body, _, _) ->
            let instanceNames = extraFromBindings bindings List.Empty |> Set.ofList
            processLetBinding instanceNames body returnEmptyArray
        | SynExpr.Sequential(_, _, expr1, expr2, _, _) ->
            processExpression expr1 (fun () -> processExpression expr2 returnEmptyArray)
        | _ -> Array.empty)
        (continuation())

let runner args =
    match args.AstNode with
    | Binding(SynBinding(_, _, _, _, _, _, _, _, _, SynExpr.LetOrUse(_, _, bindings, body, _, _), _, _, _)) ->
        let instanceNames = extraFromBindings bindings List.Empty |> Set.ofList
        processLetBinding instanceNames body returnEmptyArray
    | Match(SynMatchClause(_, _, expr, _, _, _)) ->
        processExpression expr returnEmptyArray
    | Lambda(lambda, _) ->
        processExpression lambda.Body returnEmptyArray
    | Expression(SynExpr.TryWith(tryExpr, _, _, _, _, _)) ->
        processExpression tryExpr returnEmptyArray
    | Expression(SynExpr.TryFinally(tryExpr, finallyExpr, _, _, _, _)) ->
        Array.append
            (processExpression tryExpr returnEmptyArray)
            (processExpression finallyExpr returnEmptyArray)
    | Expression(SynExpr.ComputationExpr(_, expr, _)) ->
        processExpression expr returnEmptyArray
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "FavourNonMutablePropertyInitialization"
            Identifier = Identifiers.FavourNonMutablePropertyInitialization
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
