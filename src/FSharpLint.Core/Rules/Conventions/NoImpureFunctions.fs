module FSharpLint.Rules.NoImpureFunctions

open System
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<RequireQualifiedAccess>]
type Config = {
    AllowedImpureFunctions:string list
    AdditionalImpureFunctions:string list
}

let private impureFunctionIdentifiers =
    Map.ofList
        [
            ("Array.sortInPlace", Some "Array.sort")
            ("Array.sortInPlaceBy", Some "Array.Array.sortBy")
            ("Array.sortInPlaceWith", Some "Array.sortWith")

            ("Array.set", Some "Array.updateAt")

            ("Array.fill", None)
            ("Array.blit", None)

            ("Array2D.fill", None)
            ("Array2D.blit", None)
        ]

let runner (config:Config) (args:AstNodeRuleParams) =
    let checkIfImpureIdentifier (identifier:string) (range:Range) =
        let issueWarning maybeReplacementFunction =
            match maybeReplacementFunction with
            | Some replacementFunction ->
                {
                    Range = range
                    Message = String.Format(Resources.GetString "RulesConventionsNoImpureFunctionsReplacementError", replacementFunction, identifier)
                    SuggestedFix = Some (lazy ( Some { FromText = identifier; FromRange = range; ToText = replacementFunction }))
                    TypeChecks = List.Empty
                }
            | None -> 
                {
                    Range = range
                    Message = String.Format(Resources.GetString "RulesConventionsNoImpureFunctionsError", identifier)
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }

        if List.contains identifier config.AllowedImpureFunctions then
            None
        elif List.contains identifier config.AdditionalImpureFunctions then
            issueWarning None |> Some
        else
            Map.tryFind identifier impureFunctionIdentifiers
            |> Option.filter (fun _ -> not (List.contains identifier config.AllowedImpureFunctions))
            |> Option.map issueWarning

    match args.AstNode with
    | AstNode.Identifier (identifier, range) ->
        match checkIfImpureIdentifier (String.concat "." identifier) range with
        | Some impureIdentifierWarning ->
            Array.singleton impureIdentifierWarning
        | _ ->
            Array.Empty()
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "NoImpureFunctions"
            Identifier = Identifiers.NoImpureFunctions
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
