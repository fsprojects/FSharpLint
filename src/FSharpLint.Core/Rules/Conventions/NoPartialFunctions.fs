module FSharpLint.Rules.NoPartialFunctions

open FSharp.Compiler.Range
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

type private Replacement =
    | PatternMatch
    | Function of functionName:string

let private partialFunctions =
    [
        // Option
        ("Option.get", PatternMatch)

        // List
        ("List.find", Function "List.tryFind")
    ] |> Map.ofList

let private checkIfPartial (identifier:string) (range:range) =
    Map.tryFind identifier partialFunctions
    |> Option.map (function
        | PatternMatch ->
            {
                Range = range
                Message = sprintf "Consider using pattern matching instead of partial function '%s'" identifier
                SuggestedFix = None
                TypeChecks = []
            }
        | Function replacementFunction ->
            {
                Range = range
                Message = sprintf "Consider using '%s' instead of partial function '%s'" replacementFunction identifier
                SuggestedFix = None
                TypeChecks = []
            })

let runner (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Identifier (identifier, range) ->
        checkIfPartial (String.concat "." identifier) range
        |> Option.toArray
    | _ ->
        Array.empty

let rule =
    { Name = "NoPartialFunctions"
      Identifier = Identifiers.NoPartialFunctions
      RuleConfig = { AstNodeRuleConfig.Runner = runner
                     Cleanup = ignore } }
    |> AstNodeRule

