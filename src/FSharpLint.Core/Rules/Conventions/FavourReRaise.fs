module FSharpLint.Rules.FavourReRaise

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private runner (args: AstNodeRuleParams) =
    let generateError suggestedFix range =
        { Range = range
          Message = Resources.GetString "RulesFavourReRaise"
          SuggestedFix = Some suggestedFix
          TypeChecks = List.empty }
        |> Array.singleton

    let rec checkExpr (expr) maybeIdent =
        match expr with
        | SynExpr.App (_, _, SynExpr.Ident raiseId, expression, range) when raiseId.idText = "raise" ->
            let suggestedFix = lazy(Some({ FromRange = range; FromText = raiseId.idText; ToText = "reraise()" }))
            match expression with
            | SynExpr.Ident ident ->
                match maybeIdent with
                | Some id when id = ident.idText ->
                    generateError suggestedFix range
                | _ -> Array.empty
            | SynExpr.LongIdent (_, LongIdentWithDots (id, _), _, range) -> generateError suggestedFix range
            | _ -> Array.empty
        | SynExpr.TryWith (expressions, _, clauseList, _expression, _range, _, _) as expr ->
            clauseList
            |> List.toArray
            |> Array.collect (fun clause ->
                match clause with
                | SynMatchClause (pat, _, app, _, _) ->
                    match pat with
                    | SynPat.Named (_, id, _, _, _) -> checkExpr app (Some id.idText)
                    | _ -> checkExpr app None)
        | SynExpr.IfThenElse (_, expr, _, _, _, _, _) -> checkExpr expr maybeIdent
        | _ -> Array.empty

    match args.AstNode with
    | AstNode.Expression expr -> checkExpr expr None
    | _ -> Array.empty

let rule =
    { Name = "FavourReRaise"
      Identifier = Identifiers.FavourReRaise
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
