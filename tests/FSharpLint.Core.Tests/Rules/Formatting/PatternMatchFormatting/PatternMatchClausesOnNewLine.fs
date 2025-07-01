module FSharpLint.Core.Tests.Rules.Formatting.PatternMatchClausesOnNewLine

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFormattingPatternMatchClausesOnNewLine() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(PatternMatchClausesOnNewLine.rule)

        [<Test>]
    member this.``Error for pattern match clauses on same line``() =
        this.Parse("""
module Program

match 1 with
| 1 -> 1 | 2 -> 2""")

        Assert.IsTrue(this.ErrorExistsAt(5, 11))

    [<Test>]
    member this.``No error for pattern match clauses on different lines``() =
        this.Parse("""
module Program

match 1 with
| 1 -> 1
| 2 -> 2""")

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for exception match clauses on same line``() =
        this.Parse("""
module Program

try
    2/0
with
| :? System.DivideByZeroException -> 1 | :? System.Exception -> 2 """)

        Assert.IsTrue(this.ErrorExistsAt(7, 41))

    [<Test>]
    member this.``No error for exception match clauses on same line``() =
        this.Parse("""
module Program

try
    2/0
with
| :? System.DivideByZeroException -> 1
| :? System.Exception -> 2 """)

        Assert.IsTrue(this.NoErrorsExist)

