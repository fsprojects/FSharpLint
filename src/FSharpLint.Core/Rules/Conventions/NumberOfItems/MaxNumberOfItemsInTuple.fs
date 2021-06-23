module FSharpLint.Rules.MaxNumberOfItemsInTuple

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private isInApplication (syntaxArray:AbstractSyntaxArray.Node[]) i =
    let rec isApplicationNode i =
        if i <= 0 then false
        else
            let node = syntaxArray.[i]
            match node.Actual with
            | AstNode.Expression(SynExpr.Paren(_)) -> isApplicationNode node.ParentIndex
            | AstNode.Expression(SynExpr.App(_) | SynExpr.New(_)) -> true
            | _ -> false

    if i <= 0 then false
    else isApplicationNode syntaxArray.[i].ParentIndex

let private validateTuple (maxItems:int) (items:SynExpr list) =
    if List.length items > maxItems then
        let errorFormatString = Resources.GetString("RulesNumberOfItemsTupleError")
        let error = String.Format(errorFormatString, maxItems)
        { Range = items.[maxItems].Range; Message = error; SuggestedFix = None; TypeChecks = [] } |> Array.singleton
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
    { Name = "MaxNumberOfItemsInTuple"
      Identifier = Identifiers.MaxNumberOfItemsInTuple
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule
