module FSharpLint.Core.Tests.Rules.Typography.MaxCharactersOnLine

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestTypographyMaxCharactersOnLine() =
    inherit TestLineRuleBase.TestLineRuleBase(MaxCharactersOnLine.rule { maxCharactersOnLine = 60 })
    
    [<Test>]
    member this.TooManyCharactersOnLine() = 
        this.Parse "let line = 55 + 77 + 77 + 55 + 55 + 55 + 77 + 55 + 55 + 77 + 55 + 55 + 77 + 77"

        Assert.IsTrue(this.ErrorExistsAt(1, 61))

    [<Test>]
    member this.TooManyCharactersOnLineSuppressed() = 
        this.Parse """
        [<SuppressMessage("Typography", "MaxCharactersOnLine")>]
        let line = 55 + 77 + 77"""

        Assert.IsFalse(this.ErrorExistsAt(3, 11))