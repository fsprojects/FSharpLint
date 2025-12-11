module FSharpLint.Core.Tests.TestRuleBase

open System
open System.Text
open NUnit.Framework
open FSharpLint.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Violation

[<AbstractClass>]
type TestRuleBase () =
    let violations = ResizeArray<_>()

    abstract Parse : string * ?fileName:string * ?checkFile:bool * ?globalConfig:GlobalRuleConfig -> unit

    member _.PostViolation (violation: LintViolation) =
        if not violation.Details.TypeChecks.IsEmpty then
            let successfulTypeCheck =
                (true, violation.Details.TypeChecks)
                ||> List.fold (fun acc check -> acc && check ())

            if successfulTypeCheck then
                violations.Add violation
        else
            violations.Add violation

    member _.ViolationRanges =
        violations
        |> Seq.map (fun lintViolation -> (lintViolation.Details.Range.StartLine, lintViolation.Details.Range.StartColumn))
        |> List.ofSeq

    member _.ViolationExistsAt(startLine, startColumn) =
        violations
        |> Seq.exists (fun lintViolation -> lintViolation.Details.Range.StartLine = startLine && lintViolation.Details.Range.StartColumn = startColumn)

    member _.ViolationsAt(startLine, startColumn) =
        violations
        |> Seq.filter (fun lintViolation -> lintViolation.Details.Range.StartLine = startLine && lintViolation.Details.Range.StartColumn = startColumn)

    member _.ViolationExistsOnLine(startLine) =
        violations
        |> Seq.exists (fun lintViolation -> lintViolation.Details.Range.StartLine = startLine)

    member _.NoViolationExistsOnLine(startLine) =
        violations
        |> Seq.exists (fun lintViolation -> lintViolation.Details.Range.StartLine = startLine)
        |> not

    // prevent tests from passing if errors exist, just not on the line being checked
    member _.NoViolationsExist =
        Seq.isEmpty violations

    member _.ViolationsExist =
        violations |> Seq.isEmpty |> not

    member _.ViolationMsg =
        match violations with
        | xs when xs.Count = 0 -> "No violations"
        | _ ->
            violations
            |> Seq.map (fun lintViolation ->
                $"(({lintViolation.Details.Range.StartRange.StartLine}, {lintViolation.Details.Range.StartColumn}) - ({lintViolation.Details.Range.EndRange.EndLine}, {lintViolation.Details.Range.EndRange.EndColumn}) -> {lintViolation.Details.Message})")
            |> (fun violationMsg -> String.Join("; ", violationMsg))

    member this.ViolationWithMessageExistsAt(message, startLine, startColumn) =
        this.ViolationsAt(startLine, startColumn)
        |> Seq.exists (fun lintViolation -> lintViolation.Details.Message = message)

    member _.AssertViolationWithMessageExists(message) =
        let foundViolations = violations |> Seq.map (fun linterViolation -> linterViolation.Details.Message)
        let foundViolationsStr = String.concat "," foundViolations
        Assert.IsTrue(foundViolations |> Seq.contains message, $"Couldn't find message '{message}', found: [{foundViolationsStr}]")

    member this.AssertNoViolations() =
        Assert.IsFalse(this.ViolationsExist, "Expected no violations, but was: " + this.ViolationMsg)

    member this.ApplyAutoFix (source:string) =
        let firstAutoFix =
            violations
            |> Seq.choose (fun linterViolation -> linterViolation.Details.AutoFix)
            |> Seq.tryHead

        match Option.bind (fun (autoFix: Lazy<option<AutoFix>>) -> autoFix.Value) firstAutoFix with
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
    member _.SetUp() =
        violations.Clear()
