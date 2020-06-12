module FSharpLint.Rules.NoPartialFunctions

open System
open FSharp.Compiler.Range
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

type private Replacement =
    | PatternMatch
    | Function of functionName:string

let private partialFunctionIdentifiers =
    [
        // Option
        ("Option.get", PatternMatch)

        // Array
        ("Array.exactlyOne", Function "Array.tryExactlyOne")
        ("Array.get", Function "Array.tryItem")
        ("Array.item", Function "Array.tryItem")
        ("Array.find", Function "Array.tryFind")
        ("Array.findIndex", Function "Array.tryFindIndex")
        ("Array.findBack", Function "Array.tryFindBack")
        ("Array.head", Function "Array.tryHead")
        ("Array.last", Function "Array.tryLast")
        ("Array.tail", Function "Array.tryLast")
        ("Array.reduce", Function "Array.fold")
        ("Array.reduceBack", Function "Array.foldBack")
        ("Array.pick", Function "Array.tryPick")

        // Seq
        ("Seq.exactlyOne", Function "Seq.tryExactlyOne")
        ("Seq.item", Function "Seq.tryItem")
        ("Seq.find", Function "Seq.tryFind")
        ("Seq.findIndex", Function "Seq.tryFindIndex")
        ("Seq.findBack", Function "Seq.tryFindBack")
        ("Seq.head", Function "Seq.tryHead")
        ("Seq.last", Function "Seq.tryLast")
        ("Seq.tail", Function "Seq.tryLast")
        ("Seq.reduce", Function "Seq.fold")
        ("Seq.reduceBack", Function "Seq.foldBack")
        ("Seq.pick", Function "Seq.tryPick")

        // List
        ("List.exactlyOne", Function "List.tryExactlyOne")
        ("List.item", Function "List.tryItem")
        ("List.find", Function "List.tryFind")
        ("List.findIndex", Function "List.tryFindIndex")
        ("List.findBack", Function "List.tryFindBack")
        ("List.head", Function "List.tryHead")
        ("List.last", Function "List.tryLast")
        ("List.tail", Function "List.tryLast")
        ("List.reduce", Function "List.fold")
        ("List.reduceBack", Function "List.foldBack")
        ("List.pick", Function "List.tryPick")
    ] |> Map.ofList

let private checkIfPartialIdentifier (identifier:string) (range:range) =
    Map.tryFind identifier partialFunctionIdentifiers
    |> Option.map (function
        | PatternMatch ->
            {
                Range = range
                Message = String.Format(Resources.GetString ("RulesConventionsNoPartialFunctionsPatternMatchError"), identifier)
                SuggestedFix = None
                TypeChecks = []
            }
        | Function replacementFunction ->
            {
                Range = range
                Message = String.Format(Resources.GetString "RulesConventionsNoPartialFunctionsReplacementError", replacementFunction, identifier)
                SuggestedFix = Some (lazy ( Some { FromText = identifier; FromRange = range; ToText = replacementFunction }))
                TypeChecks = []
            })

let runner (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Identifier (identifier, range) ->
        checkIfPartialIdentifier (String.concat "." identifier) range
        |> Option.toArray
    | _ ->
        Array.empty

let rule =
    { Name = "NoPartialFunctions"
      Identifier = Identifiers.NoPartialFunctions
      RuleConfig = { AstNodeRuleConfig.Runner = runner
                     Cleanup = ignore } }
    |> AstNodeRule

