module FSharpLint.Rules.Formatting.TypedItemSpacing

open System
open FSharp.Compiler.Ast
open FSharpLint.Framework
open FSharpLint.Framework.Analyser
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

type TypedItemStyle =
    | NoSpaces
    | SpaceAfter
    | SpacesAround

type Config =
    { typedItemStyle : TypedItemStyle }

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

let private expectedSpacesFromConfig (typedItemStyle : TypedItemStyle) =
    match typedItemStyle with
    | TypedItemStyle.NoSpaces -> (0, 0)
    | TypedItemStyle.SpaceAfter -> (0, 1)
    | TypedItemStyle.SpacesAround -> (1, 1)

/// Checks for correct spacing around colon of typed expression.
let private checkTypedItemSpacing (args : AstNodeRuleParams<Config>) =
    match args.astNode with
    | AstNode.Pattern (SynPat.Typed (_, _, range)) ->
        let (expectedSpacesBefore, expectedSpacesAfter) =
            expectedSpacesFromConfig args.config.typedItemStyle

        args.info.TryFindTextOfRange range
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
                    let errorFormatString = Resources.GetString("RulesFormattingTypedItemSpacingError")
                    Some
                        { Range = range
                          Message = String.Format(errorFormatString, expectedSpacesBefore, expectedSpacesAfter)
                          SuggestedFix = Some suggestedFix
                          TypeChecks = [] }
                    else
                        None
            | _ -> None)
        |> Option.toArray
    | _ -> [||]

let rule =
    { name = "TypedItemSpacing" 
      identifier = None
      runner = checkTypedItemSpacing }
