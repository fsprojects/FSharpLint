module FSharpLint.Core.Tests.Rules.Typography.TrailingNewLineInFile

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestTypographyTrailingNewLineInFile() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingNewLineInFile.rule)

    [<Test>]
    member this.NewLineOnEndOfFile() =
        this.Parse ("let dog = 9" + System.Environment.NewLine)

        Assert.IsTrue(this.ViolationExistsAt(1, 0))
