module FSharpLint.Core.Tests.TestRuleBase

open System
open System.Text
open NUnit.Framework
open FSharpLint.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Suggestion

[<AbstractClass>]
type TestRuleBase () =
    let suggestions = ResizeArray<_>()

    abstract Parse : string * ?fileName:string * ?checkFile:bool * ?globalConfig:GlobalRuleConfig -> unit

    member _.PostSuggestion (suggestion:Suggestion.LintWarning) =
        if not suggestion.Details.TypeChecks.IsEmpty then
            let successfulTypeCheck =
                (true, suggestion.Details.TypeChecks)
                ||> List.fold (fun acc check -> acc && check ())

            if successfulTypeCheck then
                suggestions.Add(suggestion)
        else
            suggestions.Add(suggestion)

    member _.ErrorRanges =
        suggestions
        |> Seq.map (fun linterSuggestion -> (linterSuggestion.Details.Range.StartLine, linterSuggestion.Details.Range.StartColumn))
        |> List.ofSeq

    member _.ErrorExistsAt(startLine, startColumn) =
        suggestions
        |> Seq.exists (fun linterSuggestion -> linterSuggestion.Details.Range.StartLine = startLine && linterSuggestion.Details.Range.StartColumn = startColumn)

    member _.ErrorsAt(startLine, startColumn) =
        suggestions
        |> Seq.filter (fun linterSuggestion -> linterSuggestion.Details.Range.StartLine = startLine && linterSuggestion.Details.Range.StartColumn = startColumn)

    member _.ErrorExistsOnLine(startLine) =
        suggestions
        |> Seq.exists (fun linterSuggestion -> linterSuggestion.Details.Range.StartLine = startLine)

    member _.NoErrorExistsOnLine(startLine) =
        suggestions
        |> Seq.exists (fun linterSuggestion -> linterSuggestion.Details.Range.StartLine = startLine)
        |> not

    // prevent tests from passing if errors exist, just not on the line being checked
    member _.NoErrorsExist =
        suggestions |> Seq.isEmpty

    member _.ErrorsExist =
        suggestions |> Seq.isEmpty |> not

    member _.ErrorMsg =
        match suggestions with
        | xs when xs.Count = 0 -> "No errors"
        | _ ->
            suggestions
            |> Seq.map (fun linterSuggestion -> 
                $"(({linterSuggestion.Details.Range.StartRange.StartLine}, {linterSuggestion.Details.Range.StartColumn}) - ({linterSuggestion.Details.Range.EndRange.EndLine}, {linterSuggestion.Details.Range.EndRange.EndColumn}) -> {linterSuggestion.Details.Message})")
            |> (fun suggestionMsg -> String.Join("; ", suggestionMsg))

    member this.ErrorWithMessageExistsAt(message, startLine, startColumn) =
        this.ErrorsAt(startLine, startColumn)
        |> Seq.exists (fun linterSuggestion -> linterSuggestion.Details.Message = message)

    member _.AssertErrorWithMessageExists(message) =
        let foundSuggestions = suggestions |> Seq.map (fun linterSuggestion -> linterSuggestion.Details.Message)
        let foundSuggestionsStr = String.concat "," foundSuggestions
        Assert.IsTrue(foundSuggestions |> Seq.contains message, $"Couldn't find message '{message}', found: [{foundSuggestionsStr}]")

    member this.AssertNoWarnings() =
        Assert.IsFalse(this.ErrorsExist, "Expected no errors, but was: " + this.ErrorMsg)

    member this.ApplyQuickFix (source:string) =
        let firstSuggestedFix =
            suggestions
            |> Seq.choose (fun linterSuggestion -> linterSuggestion.Details.SuggestedFix)
            |> Seq.tryHead

        match firstSuggestedFix |> Option.bind (fun suggestedFix -> suggestedFix.Value) with
        | Some(fix) ->
            let startIndex = ExpressionUtilities.findPos fix.FromRange.Start source
            let endIndex = ExpressionUtilities.findPos fix.FromRange.End source

            match (startIndex, endIndex) with
            | (Some(startIndex), Some(endIndex)) ->
                (StringBuilder source)
                    .Remove(startIndex, endIndex - startIndex)
                    .Insert(startIndex, fix.ToText)
                    .ToString()
            | _ -> source
        | None -> source

    [<SetUp>]
    member _.SetUp() = suggestions.Clear()