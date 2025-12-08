namespace NUnit.Framework

open FSharpLint.Core.Tests

[<AutoOpen>]
module Extensions =

    type Assert with

        static member ViolationExistsAt(matcher:TestRuleBase.TestRuleBase, (startLine, startCol)) =
            if not (matcher.ViolationExistsAt(startLine, startCol))
            then
                match matcher.ViolationRanges with
                | [] -> failwithf "Expected a violation to exist at (%d, %d), but none could be found at that location. There were no violation reported" startLine startCol
                | knownViolations -> failwithf "Expected a violation to exist at (%d, %d), but no violation could be found at that location.  Current violations were:\n%A" startLine startCol knownViolations
