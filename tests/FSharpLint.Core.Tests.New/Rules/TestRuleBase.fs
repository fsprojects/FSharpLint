module TestRuleBase

open System
open System.Text
open NUnit.Framework
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

[<AbstractClass>]
type TestRuleBase () =
    let suggestions = ResizeArray<_>()

    abstract Parse : string -> unit
        
    member __.postSuggestion (suggestion:LintWarning.Warning) =
        suggestions.Add(suggestion)

    member __.ErrorExistsAt(startLine, startColumn) =
        suggestions
        |> Seq.exists (fun s -> s.Range.StartLine = startLine && s.Range.StartColumn = startColumn)

    member __.ErrorsAt(startLine, startColumn) =
        suggestions
        |> Seq.filter (fun s -> s.Range.StartLine = startLine && s.Range.StartColumn = startColumn)

    member __.ErrorExistsOnLine(startLine) =
        suggestions
        |> Seq.exists (fun s -> s.Range.StartLine = startLine)

    member __.NoErrorExistsOnLine(startLine) =
        suggestions
        |> Seq.exists (fun s -> s.Range.StartLine = startLine)
        |> not

    // prevent tests from passing if errors exist, just not on the line being checked
    member __.NoErrorsExist =
        suggestions |> Seq.isEmpty

    member __.ErrorsExist =
        suggestions |> Seq.isEmpty |> not

    member __.ErrorMsg =
        match suggestions with
        | xs when xs.Count = 0 -> "No errors"
        | _ ->
            suggestions
            |> Seq.map (fun s -> (sprintf "((%i, %i) - (%i, %i) -> %s)"
                s.Range.StartRange.StartLine s.Range.StartColumn 
                s.Range.EndRange.EndLine s.Range.EndRange.EndColumn s.Info ))
            |> (fun x -> String.Join("; ", x))

    member this.ErrorWithMessageExistsAt(message, startLine, startColumn) =
        this.ErrorsAt(startLine, startColumn)
        |> Seq.exists (fun s -> s.Info = message)

    member __.ErrorWithMessageExists(message) =
        suggestions |> Seq.exists (fun s -> s.Info = message)

    member this.AssertNoWarnings() =
        Assert.IsFalse(this.ErrorsExist, "Expected no errors, but was: " + this.ErrorMsg)

    member this.ApplyQuickFix (source:string) =
        let firstSuggestedFix =
            suggestions 
            |> Seq.choose (fun x -> x.Fix)
            |> Seq.tryHead

        match firstSuggestedFix with
        | Some(fix) ->
            let startIndex = ExpressionUtilities.findPos fix.FromRange.Start source
            let endIndex = ExpressionUtilities.findPos fix.FromRange.End source

            match startIndex, endIndex with
            | Some(startIndex), Some(endIndex) -> 
                (StringBuilder source)
                    .Remove(startIndex, endIndex - startIndex)
                    .Insert(startIndex, fix.ToText)
                    .ToString()
            | _ -> source
        | None -> source

    [<SetUp>]
    member __.SetUp() = suggestions.Clear()