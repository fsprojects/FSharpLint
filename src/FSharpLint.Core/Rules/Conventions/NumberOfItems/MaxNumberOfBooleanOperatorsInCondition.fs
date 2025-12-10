module FSharpLint.Rules.MaxNumberOfBooleanOperatorsInCondition

open System
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private boolFunctions =
    Set.ofList ["op_BooleanOr"; "op_BooleanAnd"; "not"]

let private validateCondition (maxBooleanOperators:int) condition =
    let rec countBooleanOperators total = function
        | SynExpr.App(_, _, expr, SynExpr.Ident(ident), _)
        | SynExpr.App(_, _, SynExpr.Ident(ident), expr, _) ->
            if Set.contains ident.idText boolFunctions then
                countBooleanOperators (total + 1) expr
            else
                countBooleanOperators total expr
        | SynExpr.App(_, _, expr, expr2, _) ->
            let left = countBooleanOperators 0 expr
            let right = countBooleanOperators 0 expr2
            total + left + right
        | SynExpr.Paren(expr, _, _, _) ->
            countBooleanOperators total expr
        | ExpressionUtilities.Identifier([ ident ], _) ->
            if Set.contains ident.idText boolFunctions then
                total + 1
            else
                total
        | _ ->
            total

    let ruleName = "MaxNumberOfBooleanOperatorsInCondition"

    let numberOfBooleanOperators = countBooleanOperators 0 condition

    if numberOfBooleanOperators > maxBooleanOperators then
        let violationTextFormatString = Resources.GetString "RulesNumberOfItemsBooleanConditionsViolation"
        let violationMsg = String.Format(violationTextFormatString, maxBooleanOperators)
        Array.singleton
            {
                Range = condition.Range
                Message = violationMsg
                AutoFix = None
                TypeChecks = List.Empty
            }
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
    | AstNode.Match(SynMatchClause(_, Some(whenExpr), _, _, _, _)) ->
        validateCondition config.MaxItems whenExpr
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "MaxNumberOfBooleanOperatorsInCondition"
            Identifier = Identifiers.MaxNumberOfBooleanOperatorsInCondition
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
