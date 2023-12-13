module FSharpLint.Rules.FavourNonMutablePropertyInitialization

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

let private getWarningDetails (ident: Ident) =
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

let private extraInstanceMethod (app:SynExpr) (instanceMethodCalls: List<string>) =
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

let rec private extraFromBindings (bindings: List<SynBinding>) (classInstances: List<string>) =
    match bindings with
    | SynBinding(_, _, _, _, _, _, _, SynPat.Named(SynIdent(ident, _), _, _, _), _, _expression, _, _, _)::rest ->
        extraFromBindings rest (ident.idText::classInstances)
    | _ -> classInstances

let rec private processLetBinding (instanceNames: Set<string>) (body: SynExpr) : array<WarningDetails> =
    match body with
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
        Array.append
            (processLetBinding instanceNames expr1)
            (processLetBinding instanceNames expr2)
    | _ -> Array.empty

and processExpression (expression: SynExpr) : array<WarningDetails> =
    match expression with
    | SynExpr.LetOrUse(_, _, bindings, body, _, _) ->
        let instanceNames = extraFromBindings bindings List.Empty |> Set.ofList
        processLetBinding instanceNames body
    | SynExpr.Sequential(_, _, expr1, expr2, _, _) ->
        Array.append
            (processExpression expr1)
            (processExpression expr2)
    | _ -> Array.empty

let runner args =
    match args.AstNode with
    | Binding(SynBinding(_, _, _, _, _, _, _, _, _, SynExpr.LetOrUse(_, _, bindings, body, _, _), _, _, _)) ->
        let instanceNames = extraFromBindings bindings List.Empty |> Set.ofList
        processLetBinding instanceNames body
    | Match(SynMatchClause(_, _, expr, _, _, _)) ->
        processExpression expr
    | Lambda(lambda, _) ->
        processExpression lambda.Body
    | Expression(SynExpr.TryWith(tryExpr, _, _, _, _, _)) ->
        processExpression tryExpr
    | Expression(SynExpr.TryFinally(tryExpr, finallyExpr, _, _, _, _)) ->
        Array.append
            (processExpression tryExpr)
            (processExpression finallyExpr)
    | Expression(SynExpr.ComputationExpr(_, expr, _)) ->
        processExpression expr
    | _ -> Array.empty

let rule =
    { Name = "FavourNonMutablePropertyInitialization"
      Identifier = Identifiers.FavourNonMutablePropertyInitialization
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
