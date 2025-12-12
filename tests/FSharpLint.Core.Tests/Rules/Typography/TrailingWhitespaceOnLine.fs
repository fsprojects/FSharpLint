module FSharpLint.Core.Tests.Rules.Typography.TrailingWhitespaceOnLine

// fsharplint:disable TupleIndentation

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOn() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 0; OneSpaceAllowedAfterOperator = true; IgnoreBlankLines = false })

    [<Test>]
    member this.SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOn() =
        this.Parse("fun x -> " + System.Environment.NewLine + "    ()")

        Assert.IsFalse(this.ViolationExistsAt(1, 8))

[<TestFixture>]
type SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOff() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 0; OneSpaceAllowedAfterOperator = false; IgnoreBlankLines = false })

    [<Test>]
    member this.SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOff() =
        this.Parse("fun x -> " + System.Environment.NewLine + "    ()")

        Assert.IsTrue(this.ViolationExistsAt(1, 8))

    [<Test>]
    member this.NoSpacesOnEndOfLineWithNoSpacesAllowed() =
        this.Parse("let line = 55")

        Assert.IsFalse(this.ViolationExistsAt(1, 13))

    [<Test>]
    member this.OneSpaceOnEndOfLineWithNoSpacesAllowed() =
        this.Parse("let line = 55 ")

        Assert.IsTrue(this.ViolationExistsAt(1, 13))

    [<Test>]
    member this.WhitespaceOnEndOfLineAfterNewLine() =
        this.Parse (System.Environment.NewLine + "let d = 0 ")

        Assert.IsTrue(this.ViolationExistsAt(2, 9))


[<TestFixture>]
type MultipleSpacesOnEndOfLineAfterOperatorWithConfigPropertyOn() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 0; OneSpaceAllowedAfterOperator = true; IgnoreBlankLines = false })

    [<Test>]
    member this.MultipleSpacesOnEndOfLineAfterOperatorWithConfigPropertyOn() =
        this.Parse("fun x ->  " + System.Environment.NewLine + "    ()")

        Assert.IsTrue(this.ViolationExistsAt(1, 8))

[<TestFixture>]
type TestOneSpace() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 1; OneSpaceAllowedAfterOperator = false; IgnoreBlankLines = false })

    [<Test>]
    member this.OneSpaceOnEndOfLineWithOneSpaceAllowed() =
        this.Parse("let line = 55 ")

        Assert.IsFalse(this.ViolationExistsAt(1, 14))
        Assert.IsFalse(this.ViolationExistsAt(1, 13))

    [<Test>]
    member this.TwoSpacesOnEndOfLineWithOneSpaceAllowed() =
        this.Parse("let line = 55  ")

        Assert.IsTrue(this.ViolationExistsAt(1, 13))

[<TestFixture>]
type TwoSpacesOnEndOfLineWithTwoSpacesAllowed() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 2; OneSpaceAllowedAfterOperator = false; IgnoreBlankLines = false })

    [<Test>]
    member this.TwoSpacesOnEndOfLineWithTwoSpacesAllowed() =
        this.Parse("let line = 55  ")

        Assert.IsFalse(this.ViolationExistsAt(1, 15))
        Assert.IsFalse(this.ViolationExistsAt(1, 14))
        Assert.IsFalse(this.ViolationExistsAt(1, 13))

[<TestFixture>]
type WhitespaceEntireLine() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 0; OneSpaceAllowedAfterOperator = false; IgnoreBlankLines = false })

    [<Test>]
    member this.WhitespaceEntireLine() =
        this.Parse " "

        Assert.IsTrue(this.ViolationExistsAt(1, 0))

[<TestFixture>]
type WhitespaceEntireLineIgnoreBlankLines() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { NumberOfSpacesAllowed = 0; OneSpaceAllowedAfterOperator = false; IgnoreBlankLines = true })

    [<Test>]
    member this.WhitespaceEntireLineIgnoreBlankLines() =
        this.Parse(" ")

        Assert.IsFalse(this.ViolationExistsAt(1, 0))
