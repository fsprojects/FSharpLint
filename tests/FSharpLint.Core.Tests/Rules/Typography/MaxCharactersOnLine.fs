module FSharpLint.Core.Tests.Rules.Typography.MaxCharactersOnLine

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestTypographyMaxCharactersOnLine() =
    inherit TestLineRuleBase.TestLineRuleBase(MaxCharactersOnLine.rule { MaxCharactersOnLine = 60 })

    [<Test>]
    member this.TooManyCharactersOnLine() =
        this.Parse "let line = 55 + 77 + 77 + 55 + 55 + 55 + 77 + 55 + 55 + 77 + 55 + 55 + 77 + 77"

        Assert.IsTrue(this.ViolationExistsAt(1, 61))
