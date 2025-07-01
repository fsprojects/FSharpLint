namespace NUnit.Framework

open FSharpLint.Core.Tests

[<AutoOpen>]
module Extensions =

    type Assert with

        static member ErrorExistsAt(matcher:TestRuleBase.TestRuleBase, (startLine, startCol)) =
            if not (matcher.ErrorExistsAt(startLine, startCol))
            then
                match matcher.ErrorRanges with
                | [] -> failwithf "Expected an error to exist at (%d, %d), but no error could be found at that location. There were no errors reported" startLine startCol
                | knownErrors -> failwithf "Expected an error to exist at (%d, %d), but no error could be found at that location.  Current errors are:\n%A" startLine startCol knownErrors
