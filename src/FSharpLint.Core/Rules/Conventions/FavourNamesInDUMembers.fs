module FSharpLint.Rules.FavourNamesInDUMembers

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    let emitWarning range =
        Array.singleton 
            { 
                Range = range
                Message = Resources.GetString "RulesFavourNamesInDUMembers"
                SuggestedFix = None
                TypeChecks = List.Empty 
            }

    let getFields (case: SynUnionCase) =
        match case with 
        | SynUnionCase(_, _, SynUnionCaseKind.Fields(fields), _, _, _, _) ->
            fields
        | _ -> List.empty

    match args.AstNode with
    | AstNode.TypeSimpleRepresentation(SynTypeDefnSimpleRepr.Union(_, cases, _)) ->
        let fields =
            cases
            |> List.collect getFields

        if cases.Length = 1 && fields.Length = 1 then
            Array.empty
        else
            fields
            |> Array.ofList
            |> Array.collect 
                (fun field ->
                    match field with
                    | SynField(_, false, None, _, false, _, _, range, _) -> emitWarning range
                    | _ -> Array.empty)
    | _ ->
        Array.empty

let rule =
    AstNodeRule
        {
            Name = "FavourNamesInDUMembers"
            Identifier = Identifiers.FavourNamesInDUMembers
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
