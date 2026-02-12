module FSharpLint.Rules.MaxNumberOfBooleanOperatorsInCondition

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

// There is no need to recreate the set on every function call.
//fsharplint:disable FavourLocalOverPrivate
let private boolFunctions =
    Set.ofList ["op_BooleanOr"; "op_BooleanAnd"; "not"]
//fsharplint:enable FavourLocalOverPrivate

[<TailCall>]
let rec private countBooleanOperators total expressions = 
    match expressions with
    | SynExpr.App(_, _, expr, SynExpr.Ident(ident), _) :: rest
    | SynExpr.App(_, _, SynExpr.Ident(ident), expr, _) :: rest ->
        if Set.contains ident.idText boolFunctions then
            countBooleanOperators (total + 1) (expr :: rest)
        else
            countBooleanOperators total (expr :: rest)
    | SynExpr.App(_, _, expr, expr2, _) :: rest ->
        countBooleanOperators total (expr :: expr2 :: rest)
    | SynExpr.Paren(expr, _, _, _) :: rest ->
        countBooleanOperators total (expr :: rest)
    | ExpressionUtilities.Identifier([ ident ], _) :: rest ->
        if Set.contains ident.idText boolFunctions then
            countBooleanOperators (total + 1) rest
        else
            countBooleanOperators total rest
    | _ :: rest ->
        countBooleanOperators total rest
    | [] -> total

let rule config =
    let validateCondition (maxBooleanOperators:int) condition =
        let numberOfBooleanOperators = countBooleanOperators 0 (List.singleton condition)

        if numberOfBooleanOperators > maxBooleanOperators then
            let errorFormatString = Resources.GetString("RulesNumberOfItemsBooleanConditionsError")
            let error = String.Format(errorFormatString, maxBooleanOperators)
            Array.singleton
                {
                    Range = condition.Range
                    Message = error
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }
        else
            Array.empty

    let runner (ruleConfig: Helper.NumberOfItems.Config) (args:AstNodeRuleParams) =
        match args.AstNode with
        | AstNode.Expression(expression) ->
            match expression with
            | SynExpr.IfThenElse(condition, _, _, _, _, _, _)
            | SynExpr.While(_, condition, _, _)
            | SynExpr.Assert(condition, _) ->
                validateCondition ruleConfig.MaxItems condition
            | _ -> Array.empty
        | AstNode.Match(SynMatchClause(_, Some(whenExpr), _, _, _, _)) ->
            validateCondition ruleConfig.MaxItems whenExpr
        | _ -> Array.empty

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
