module FSharpLint.Core.Tests.Rules.Typography.TrailingWhitespaceOnLine

// fsharplint:disable TupleIndentation

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type internal SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOn() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 0; OneSpaceAllowedAfterOperator = true; IgnoreBlankLines = false })

    [<Test>]
    member this.SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOn() =
        this.Parse("fun x -> " + System.Environment.NewLine + "    ()")

        Assert.IsFalse(this.ErrorExistsAt(1, 8))

[<TestFixture>]
type internal SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOff() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 0; OneSpaceAllowedAfterOperator = false; IgnoreBlankLines = false })

    [<Test>]
    member this.SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOff() =
        this.Parse("fun x -> " + System.Environment.NewLine + "    ()")

        Assert.IsTrue(this.ErrorExistsAt(1, 8))

    [<Test>]
    member this.NoSpacesOnEndOfLineWithNoSpacesAllowed() =
        this.Parse("let line = 55")

        Assert.IsFalse(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.OneSpaceOnEndOfLineWithNoSpacesAllowed() =
        this.Parse("let line = 55 ")

        Assert.IsTrue(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.WhitespaceOnEndOfLineAfterNewLine() =
        this.Parse (System.Environment.NewLine + "let d = 0 ")

        Assert.IsTrue(this.ErrorExistsAt(2, 9))


[<TestFixture>]
type internal MultipleSpacesOnEndOfLineAfterOperatorWithConfigPropertyOn() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 0; OneSpaceAllowedAfterOperator = true; IgnoreBlankLines = false })

    [<Test>]
    member this.MultipleSpacesOnEndOfLineAfterOperatorWithConfigPropertyOn() =
        this.Parse("fun x ->  " + System.Environment.NewLine + "    ()")

        Assert.IsTrue(this.ErrorExistsAt(1, 8))

[<TestFixture>]
type internal TestOneSpace() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 1; OneSpaceAllowedAfterOperator = false; IgnoreBlankLines = false })

    [<Test>]
    member this.OneSpaceOnEndOfLineWithOneSpaceAllowed() =
        this.Parse("let line = 55 ")

        Assert.IsFalse(this.ErrorExistsAt(1, 14))
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.TwoSpacesOnEndOfLineWithOneSpaceAllowed() =
        this.Parse("let line = 55  ")

        Assert.IsTrue(this.ErrorExistsAt(1, 13))

[<TestFixture>]
type internal TwoSpacesOnEndOfLineWithTwoSpacesAllowed() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 2; OneSpaceAllowedAfterOperator = false; IgnoreBlankLines = false })

    [<Test>]
    member this.TwoSpacesOnEndOfLineWithTwoSpacesAllowed() =
        this.Parse("let line = 55  ")

        Assert.IsFalse(this.ErrorExistsAt(1, 15))
        Assert.IsFalse(this.ErrorExistsAt(1, 14))
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

[<TestFixture>]
type internal WhitespaceEntireLine() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 0; OneSpaceAllowedAfterOperator = false; IgnoreBlankLines = false })

    [<Test>]
    member this.WhitespaceEntireLine() =
        this.Parse " "

        Assert.IsTrue(this.ErrorExistsAt(1, 0))

[<TestFixture>]
type internal WhitespaceEntireLineIgnoreBlankLines() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 0; OneSpaceAllowedAfterOperator = false; IgnoreBlankLines = true })

    [<Test>]
    member this.WhitespaceEntireLineIgnoreBlankLines() =
        this.Parse(" ")

        Assert.IsFalse(this.ErrorExistsAt(1, 0))