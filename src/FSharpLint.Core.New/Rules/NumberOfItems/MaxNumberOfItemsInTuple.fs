module FSharpLint.Rules.MaxNumberOfItemsInTuple

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<RequireQualifiedAccess>]
type Config =
    { maxItems : int }
    
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
        let errorFormatString = Resources.GetString("RulesNumberOfItemsTupleError")
        let error = String.Format(errorFormatString, maxItems)
        { Range = items.[maxItems].Range; Message = error; SuggestedFix = None; TypeChecks = [] } |> Array.singleton
    else
        Array.empty
        
let runner (config:Config) (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.Expression (expression) ->
        match expression with
        | SynExpr.Tuple(_, expressions, _, _) when not <| isInApplication args.syntaxArray args.skipArray args.nodeIndex ->
            validateTuple config.maxItems expressions
        | _ -> Array.empty
    | _ ->
        Array.empty
        
let rule config =
    { name = "MaxNumberOfItemsInTuple"
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner config; cleanup = ignore } }
    |> AstNodeRule
