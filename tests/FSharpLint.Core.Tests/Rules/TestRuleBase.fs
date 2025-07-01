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
        |> Seq.map (fun s -> (s.Details.Range.StartLine, s.Details.Range.StartColumn))
        |> List.ofSeq

    member _.ErrorExistsAt(startLine, startColumn) =
        suggestions
        |> Seq.exists (fun s -> s.Details.Range.StartLine = startLine && s.Details.Range.StartColumn = startColumn)

    member _.ErrorsAt(startLine, startColumn) =
        suggestions
        |> Seq.filter (fun s -> s.Details.Range.StartLine = startLine && s.Details.Range.StartColumn = startColumn)

    member _.ErrorExistsOnLine(startLine) =
        suggestions
        |> Seq.exists (fun s -> s.Details.Range.StartLine = startLine)

    member _.NoErrorExistsOnLine(startLine) =
        suggestions
        |> Seq.exists (fun s -> s.Details.Range.StartLine = startLine)
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
            |> Seq.map (fun s -> (sprintf "((%i, %i) - (%i, %i) -> %s)"
                s.Details.Range.StartRange.StartLine s.Details.Range.StartColumn
                s.Details.Range.EndRange.EndLine s.Details.Range.EndRange.EndColumn s.Details.Message ))
            |> (fun x -> String.Join("; ", x))

    member this.ErrorWithMessageExistsAt(message, startLine, startColumn) =
        this.ErrorsAt(startLine, startColumn)
        |> Seq.exists (fun s -> s.Details.Message = message)

    member _.AssertErrorWithMessageExists(message) =
        let foundSuggestions = suggestions |> Seq.map (fun s -> s.Details.Message)
        Assert.IsTrue(foundSuggestions |> Seq.contains message, sprintf "Couldn't find message '%s', found: [%s]" message (String.concat "," foundSuggestions))

    member this.AssertNoWarnings() =
        Assert.IsFalse(this.ErrorsExist, "Expected no errors, but was: " + this.ErrorMsg)

    member this.ApplyQuickFix (source:string) =
        let firstSuggestedFix =
            suggestions
            |> Seq.choose (fun x -> x.Details.SuggestedFix)
            |> Seq.tryHead

        match firstSuggestedFix |> Option.bind (fun x -> x.Value) with
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