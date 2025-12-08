module FSharpLint.Core.Tests.Rules.Formatting.PatternMatchClausesOnNewLine

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFormattingPatternMatchClausesOnNewLine() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(PatternMatchClausesOnNewLine.rule)

        [<Test>]
    member this.``Violation for pattern match clauses on same line``() =
        this.Parse("""
module Program

match 1 with
| 1 -> 1 | 2 -> 2""")

        Assert.IsTrue(this.ViolationExistsAt(5, 11))

    [<Test>]
    member this.``No violation for pattern match clauses on different lines``() =
        this.Parse("""
module Program

match 1 with
| 1 -> 1
| 2 -> 2""")

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``Violation for exception match clauses on same line``() =
        this.Parse("""
module Program

try
    2/0
with
| :? System.DivideByZeroException -> 1 | :? System.Exception -> 2 """)

        Assert.IsTrue(this.ViolationExistsAt(7, 41))

    [<Test>]
    member this.``No violation for exception match clauses on same line``() =
        this.Parse("""
module Program

try
    2/0
with
| :? System.DivideByZeroException -> 1
| :? System.Exception -> 2 """)

        Assert.IsTrue(this.NoViolationsExist)

