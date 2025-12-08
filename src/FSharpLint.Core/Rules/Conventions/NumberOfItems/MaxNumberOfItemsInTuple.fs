module FSharpLint.Rules.MaxNumberOfItemsInTuple

open System
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private isInApplication (syntaxArray:AbstractSyntaxArray.Node[]) index =
    let rec isApplicationNode nodeIndex =
        if nodeIndex <= 0 then false
        else
            let node = syntaxArray.[nodeIndex]
            match node.Actual with
            | AstNode.Expression(SynExpr.Paren(_)) -> isApplicationNode node.ParentIndex
            | AstNode.Expression(SynExpr.App(_) | SynExpr.New(_)) -> true
            | _ -> false

    if index <= 0 then false
    else isApplicationNode syntaxArray.[index].ParentIndex

let private validateTuple (maxItems:int) (items:SynExpr list) =
    if List.length items > maxItems then
        let violationTextFormatString = Resources.GetString "RulesNumberOfItemsTupleViolation"
        let violationMsg = String.Format(violationTextFormatString, maxItems)
        Array.singleton
            {
                Range = items.[maxItems].Range
                Message = violationMsg
                SuggestedFix = None
                TypeChecks = List.Empty
            }
    else
        Array.empty

let runner (config:Helper.NumberOfItems.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression (expression) ->
        match expression with
        | SynExpr.Tuple(_, expressions, _, _) when not <| isInApplication args.SyntaxArray args.NodeIndex ->
            validateTuple config.MaxItems expressions
        | _ -> Array.empty
    | _ ->
        Array.empty

let rule config =
    AstNodeRule
        {
            Name = "MaxNumberOfItemsInTuple"
            Identifier = Identifiers.MaxNumberOfItemsInTuple
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
