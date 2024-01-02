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

let traverseSynModule (declarations: List<SynModuleDecl>) (identifiers: List<string>) =
    let containsInstance (instance: string) (instances: List<string>) =
        List.exists (fun elem -> elem = instance) instances

    let extraInstanceMethod (app:SynExpr) (instanceMethodCalls: List<string>) =
        match app with
        | SynExpr.App(_, _, expression, _, _) ->
            match expression with
            | SynExpr.LongIdent(_, LongIdentWithDots(identifiers, _), _, _) ->
                match List.tryLast identifiers with
                | Some _ ->
                    identifiers.[0].idText::instanceMethodCalls
                | _ -> instanceMethodCalls
            | _ -> instanceMethodCalls
        | _ -> instanceMethodCalls

    let rec extraFromBindings (bindings: List<SynBinding>) (classInstances: List<string>) =
        match bindings with
        | SynBinding(_, _, _, _, _, _, _, SynPat.Named(_, ident, _, _, _), _, _expression, _, _)::rest ->
            extraFromBindings rest (ident.idText::classInstances)
        | _ -> classInstances

    let rec traverse (declarations: List<SynModuleDecl>) (classInstances: List<string>) (instancesWithMethodCall: List<string>) =
        match declarations with
        | SynModuleDecl.DoExpr(_, expression, _)::rest ->
            match expression with
            | SynExpr.LongIdentSet(LongIdentWithDots(identifiers, _), _, _) ->
                match List.tryLast identifiers with
                | None ->
                    Array.empty
                | Some last ->
                    if containsInstance identifiers.[0].idText classInstances then
                        if not (containsInstance identifiers.[0].idText instancesWithMethodCall) then
                            getWarningDetails last
                        else
                            Array.empty
                    else
                        Array.empty
            | SynExpr.App(_, _, expression, _, _) ->
                match expression with
                | SynExpr.LongIdent(_, LongIdentWithDots(identifiers, _), _, _) ->
                    match List.tryLast identifiers with
                    | Some _ ->
                        traverse rest classInstances (identifiers.[0].idText::instancesWithMethodCall)
                    | _ -> traverse rest classInstances instancesWithMethodCall
                | _ -> traverse rest classInstances instancesWithMethodCall
            | _ -> Array.empty
        | SynModuleDecl.NestedModule(_componentInfo, _, moduleDeclarations, _, _)::_ ->
            traverse moduleDeclarations classInstances instancesWithMethodCall
        | SynModuleDecl.Let(_, bindings, _)::rest ->
            let issueWargnings (bindings: List<SynBinding>) (identifiers: LongIdent) (restOfBindings: List<SynBinding>) (maybeApp: Option<SynExpr>) =
                let instanceMethodCall = 
                    match maybeApp with
                    | Some app -> extraInstanceMethod app instancesWithMethodCall
                    | None -> instancesWithMethodCall
                let instances = extraFromBindings bindings classInstances
                match List.tryLast identifiers with
                | None ->
                    traverse rest (extraFromBindings restOfBindings classInstances) instancesWithMethodCall
                | Some last ->
                    if containsInstance identifiers.[0].idText instances then
                        if not (containsInstance identifiers.[0].idText instanceMethodCall) then
                            getWarningDetails last
                        else
                            Array.empty
                    else
                        traverse rest (extraFromBindings restOfBindings classInstances) instancesWithMethodCall

            match bindings with
            | SynBinding(_, _, _, _, _, _, _, _, _, SynExpr.LetOrUse(_, _, bindings, SynExpr.LongIdentSet(LongIdentWithDots(identifiers, _), app, _), _), _, _)::nrest
            | SynBinding(_, _, _, _, _, _, _, _, _, SynExpr.LetOrUse(_, _, bindings, SynExpr.Sequential(_, _, app, SynExpr.LongIdentSet(LongIdentWithDots(identifiers, _), _, _), _), _), _, _)::nrest when identifiers.Length > 1 ->
                match app with
                | SynExpr.LongIdentSet(LongIdentWithDots(headIdentifiers, _), _, _) when headIdentifiers.Length > 1 ->
                    Array.append
                        (issueWargnings bindings headIdentifiers List.empty None)
                        (issueWargnings bindings identifiers nrest None)
                | _ ->
                    issueWargnings bindings identifiers nrest (Some app)
            | SynBinding(_, _, _, _, _, _, _, _, _, SynExpr.LetOrUse(_, _, bindings, SynExpr.Sequential(_, _, SynExpr.LongIdentSet(LongIdentWithDots(identifiers, _), _, _), _, _), _), _, _)::nrest when identifiers.Length > 1 ->
                issueWargnings bindings identifiers nrest None
            | _ -> traverse rest (extraFromBindings bindings classInstances) instancesWithMethodCall
        | _::rest -> traverse rest classInstances instancesWithMethodCall
        | [] -> Array.empty


    traverse declarations identifiers List.Empty

let runner args =
    match args.AstNode with
    | ModuleOrNamespace(SynModuleOrNamespace(_, _, _, moduleDeclarations, _, _, _, _)) ->
        traverseSynModule moduleDeclarations List.Empty
    | _ -> Array.empty

let rule =
    { Name = "FavourNonMutablePropertyInitialization"
      Identifier = Identifiers.FavourNonMutablePropertyInitialization
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
