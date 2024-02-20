module TestRuleBase

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

    member __.PostSuggestion (suggestion:Suggestion.LintWarning) =
        if not suggestion.Details.TypeChecks.IsEmpty then
            let successfulTypeCheck =
                (true, suggestion.Details.TypeChecks)
                ||> List.fold (fun acc check -> acc && check ())

            if successfulTypeCheck then
                suggestions.Add(suggestion)
        else
            suggestions.Add(suggestion)

    member __.ErrorRanges =
        suggestions
        |> Seq.map (fun linterSuggestion -> (linterSuggestion.Details.Range.StartLine, linterSuggestion.Details.Range.StartColumn))
        |> List.ofSeq

    member __.ErrorExistsAt(startLine, startColumn) =
        Seq.exists (fun linterSuggestion -> linterSuggestion.Details.Range.StartLine = startLine && linterSuggestion.Details.Range.StartColumn = startColumn) suggestions

    member __.ErrorsAt(startLine, startColumn) =
        Seq.filter (fun linterSuggestion -> linterSuggestion.Details.Range.StartLine = startLine && linterSuggestion.Details.Range.StartColumn = startColumn) suggestions

    member __.ErrorExistsOnLine(startLine) =
        Seq.exists (fun linterSuggestion -> linterSuggestion.Details.Range.StartLine = startLine) suggestions

    member __.NoErrorExistsOnLine(startLine) =
        suggestions
        |> Seq.exists (fun linterSuggestion -> linterSuggestion.Details.Range.StartLine = startLine)
        |> not

    // prevent tests from passing if errors exist, just not on the line being checked
    member __.NoErrorsExist =
        Seq.isEmpty suggestions

    member __.ErrorsExist =
        suggestions |> Seq.isEmpty |> not

    member __.ErrorMsg =
        match suggestions with
        | xs when xs.Count = 0 -> "No errors"
        | _ ->
            suggestions
            |> Seq.map (fun linterSuggestion -> (sprintf "((%i, %i) - (%i, %i) -> %s)"
                linterSuggestion.Details.Range.StartRange.StartLine linterSuggestion.Details.Range.StartColumn
                linterSuggestion.Details.Range.EndRange.EndLine linterSuggestion.Details.Range.EndRange.EndColumn linterSuggestion.Details.Message ))
            |> (fun suggestionMsg -> String.Join("; ", suggestionMsg))

    member this.ErrorWithMessageExistsAt(message, startLine, startColumn) =
        this.ErrorsAt(startLine, startColumn)
        |> Seq.exists (fun linterSuggestion -> linterSuggestion.Details.Message = message)

    member __.AssertErrorWithMessageExists(message) =
        let foundSuggestions = Seq.map (fun linterSuggestion -> linterSuggestion.Details.Message) suggestions
        Assert.IsTrue(Seq.contains message foundSuggestions, sprintf "Couldn't find message '%s', found: [%s]" message (String.concat "," foundSuggestions))

    member this.AssertNoWarnings() =
        Assert.IsFalse(this.ErrorsExist, "Expected no errors, but was: " + this.ErrorMsg)

    member this.ApplyQuickFix (source:string) =
        let firstSuggestedFix =
            suggestions
            |> Seq.choose (fun linterSuggestion -> linterSuggestion.Details.SuggestedFix)
            |> Seq.tryHead

        match Option.bind (fun (suggestedFix: Lazy<option<SuggestedFix>>) -> suggestedFix.Value) firstSuggestedFix with
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
    member __.SetUp() = suggestions.Clear()