module FSharpLint.Rules.MaxNumberOfBooleanOperatorsInCondition

open System
open FSharpLint.Framework
open FSharpLint.Framework.Analyser
open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<RequireQualifiedAccess>]
type Config =
    { maxBooleanOperators : int }
    
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

    let ruleName = "MaxNumberOfBooleanOperatorsInCondition"

    let numberOfBooleanOperators = countBooleanOperators 0 condition

    if numberOfBooleanOperators > maxBooleanOperators then
        let errorFormatString = Resources.GetString("RulesNumberOfItemsBooleanConditionsError")
        let error = String.Format(errorFormatString, maxBooleanOperators)
        { Range = condition.Range; Message = error; SuggestedFix = None; TypeChecks = [] } |> Array.singleton
    else
        Array.empty

let private runner (config:Config) (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.Expression(expression) ->
        match expression with
        | SynExpr.IfThenElse(condition, _, _, _, _, _, _)
        | SynExpr.While(_, condition, _, _)
        | SynExpr.Assert(condition, _) ->
            validateCondition config.maxBooleanOperators condition
        | _ -> Array.empty
    | AstNode.Match(SynMatchClause.Clause(_, Some(whenExpr), _, _, _)) ->
        validateCondition config.maxBooleanOperators whenExpr
    | _ -> Array.empty
    
let rule config =
    { name = "MaxNumberOfBooleanOperatorsInCondition"
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner config } }
    |> AstNodeRule
