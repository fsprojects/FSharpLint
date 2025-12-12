module FSharpLint.Core.Tests.Rules.Formatting.PatternMatchOrClausesOnNewLine

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFormattingPatternMatchOrClausesOnNewLine() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(PatternMatchOrClausesOnNewLine.rule)
    
    [<Test>]
    member this.``Violation for pattern match or clauses on same line``() =
        this.Parse("""
module Program

match 1 with
| 1 | 2 -> 2""")

        Assert.IsTrue(this.ViolationExistsAt(5, 6))

    [<Test>]
    member this.``No violation for pattern match or clauses on different lines``() =
        this.Parse("""
module Program

match 1 with
| 1
| 2 -> 2""")

        Assert.IsTrue(this.NoViolationsExist)
