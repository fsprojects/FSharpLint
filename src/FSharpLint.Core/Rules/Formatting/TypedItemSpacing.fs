module FSharpLint.Rules.TypedItemSpacing

open System
open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

type TypedItemStyle =
    | NoSpaces = 0
    | SpaceAfter = 1
    | SpacesAround = 2

[<RequireQualifiedAccess>]
type Config = { TypedItemStyle : TypedItemStyle }

let private getLeadingSpaces (s:string) =
    let rec loop i =
        if i < s.Length && s.[i] = ' '
        then loop (i + 1)
        else i

    loop 0

let private getTrailingSpaces (s:string) =
    let rec loop i count =
        if i >= 0 && s.[i] = ' '
        then loop (i - 1) (count + 1)
        else count

    (loop (s.Length - 1) 0)

let private expectedSpacesFromConfig (typedItemStyle:TypedItemStyle) =
    match typedItemStyle with
    | TypedItemStyle.NoSpaces -> (0, 0)
    | TypedItemStyle.SpaceAfter -> (0, 1)
    | TypedItemStyle.SpacesAround -> (1, 1)
    | _ -> (0, 0)

/// Checks for correct spacing around colon of typed expression.
let runner (config:Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Pattern (SynPat.Typed (_, _, range)) ->
        let (expectedSpacesBefore, expectedSpacesAfter) =
            expectedSpacesFromConfig config.TypedItemStyle

        ExpressionUtilities.tryFindTextOfRange range args.FileContent
        |> Option.bind (fun text ->
            match text.Split(':') with
            | [|otherText; typeText|] ->
                let spacesBeforeColon = getTrailingSpaces otherText
                let spacesAfterColon = getLeadingSpaces typeText
                if spacesBeforeColon <> expectedSpacesBefore || spacesAfterColon <> expectedSpacesAfter then
                    let trimmedOtherText = otherText.TrimEnd(' ')
                    let trimmedTypeText = typeText.TrimStart(' ')
                    let spacesBeforeString = " " |> String.replicate expectedSpacesBefore
                    let spacesAfterString = " " |> String.replicate expectedSpacesAfter
                    let suggestedFix = lazy(
                        { FromRange = range; FromText = text; ToText = trimmedOtherText + spacesBeforeString + ":" + spacesAfterString + trimmedTypeText }
                        |> Some)
                    Some
                        { Range = range
                          Message = Resources.Format("RulesFormattingTypedItemSpacingError", expectedSpacesBefore, expectedSpacesAfter)
                          SuggestedFix = Some suggestedFix
                          TypeChecks = [] }
                    else
                        None
            | _ -> None)
        |> Option.toArray
    | _ -> [||]

let rule config =
    { Name = "TypedItemSpacing"
      Identifier = Identifiers.TypedItemSpacing
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule