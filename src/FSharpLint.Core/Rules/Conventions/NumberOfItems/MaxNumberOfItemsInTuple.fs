module FSharpLint.Rules.MaxNumberOfItemsInTuple

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private isInApplication (syntaxArray:AbstractSyntaxArray.Node[]) (skipArray:AbstractSyntaxArray.Skip[]) i =
    let rec isApplicationNode i =
        if i <= 0 then false
        else
            match syntaxArray.[i].Actual with
            | AstNode.Expression(SynExpr.Paren(_)) -> isApplicationNode skipArray.[i].ParentIndex
            | AstNode.Expression(SynExpr.App(_) | SynExpr.New(_)) -> true
            | _ -> false

    if i <= 0 then false
    else isApplicationNode skipArray.[i].ParentIndex

let private validateTuple (maxItems:int) (items:SynExpr list) =
    if List.length items > maxItems then
        let error = Resources.Format("RulesNumberOfItemsTupleError", maxItems)
        {
            Range = items.[maxItems].Range
            Message = error
            SuggestedFix = None
            TypeChecks = []
        } |> Array.singleton
    else
        Array.empty

let runner (config:Helper.NumberOfItems.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression (expression) ->
        match expression with
        | SynExpr.Tuple(_, expressions, _, _) when not <| isInApplication args.SyntaxArray args.SkipArray args.NodeIndex ->
            validateTuple config.MaxItems expressions
        | _ -> Array.empty
    | _ ->
        Array.empty

let rule config =
    { Name = "MaxNumberOfItemsInTuple"
      Identifier = Identifiers.MaxNumberOfItemsInTuple
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule
