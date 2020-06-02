module FSharpLint.Rules.MaxNumberOfBooleanOperatorsInCondition

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private validateCondition (maxBooleanOperators:int) condition =
    let rec countBooleanOperators total = function
        | SynExpr.App(_, _, expr, SynExpr.Ident(ident), _)
        | SynExpr.App(_, _, SynExpr.Ident(ident), expr, _) ->
            if List.exists ((=) ident.idText) ["op_BooleanOr"; "op_BooleanAnd"; "not"] then
                countBooleanOperators (total + 1) expr
            else
                countBooleanOperators total expr
        | SynExpr.App(_, _, expr, expr2, _) ->
            total + countBooleanOperators 0 expr + countBooleanOperators 0 expr2
        | SynExpr.Paren(expr, _, _, _) ->
            countBooleanOperators total expr
        | _ -> total

    let numberOfBooleanOperators = countBooleanOperators 0 condition

    if numberOfBooleanOperators > maxBooleanOperators then
        let error = Resources.Format("RulesNumberOfItemsBooleanConditionsError", maxBooleanOperators)
        { Range = condition.Range; Message = error; SuggestedFix = None; TypeChecks = [] } |> Array.singleton
    else
        Array.empty

let private runner (config:Helper.NumberOfItems.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(expression) ->
        match expression with
        | SynExpr.IfThenElse(condition, _, _, _, _, _, _)
        | SynExpr.While(_, condition, _, _)
        | SynExpr.Assert(condition, _) ->
            validateCondition config.MaxItems condition
        | _ -> Array.empty
    | AstNode.Match(SynMatchClause.Clause(_, Some(whenExpr), _, _, _)) ->
        validateCondition config.MaxItems whenExpr
    | _ -> Array.empty

let rule config =
    { Name = "MaxNumberOfBooleanOperatorsInCondition"
      Identifier = Identifiers.MaxNumberOfBooleanOperatorsInCondition
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule
