module FSharpLint.Rules.MaxNumberOfItemsInTuple

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<TailCall>]
let rec private isApplicationNode (syntaxArray: array<AbstractSyntaxArray.Node>) nodeIndex =
    if nodeIndex <= 0 then false
    else
        let node = syntaxArray.[nodeIndex]
        match node.Actual with
        | AstNode.Expression(SynExpr.Paren(_)) -> isApplicationNode syntaxArray node.ParentIndex
        | AstNode.Expression(SynExpr.App(_) | SynExpr.New(_)) -> true
        | _ -> false

let runner (config:Helper.NumberOfItems.Config) (args:AstNodeRuleParams) =
    let isInApplication (syntaxArray:AbstractSyntaxArray.Node[]) index =        
        if index <= 0 then false
        else isApplicationNode syntaxArray syntaxArray.[index].ParentIndex

    let validateTuple (maxItems:int) (items:SynExpr list) =
        if List.length items > maxItems then
            let errorFormatString = Resources.GetString("RulesNumberOfItemsTupleError")
            let error = String.Format(errorFormatString, maxItems)
            Array.singleton
                {
                    Range = items.[maxItems].Range
                    Message = error
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }
        else
            Array.empty

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
