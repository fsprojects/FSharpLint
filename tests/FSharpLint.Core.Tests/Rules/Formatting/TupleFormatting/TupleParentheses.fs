module FSharpLint.Core.Tests.Rules.Formatting.TupleParentheses

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFormattingTupleParentheses() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(TupleParentheses.rule)

    [<Test>]
    member this.``Violation for tuple instantiation without parentheses``() =
        this.Parse("""
module Program

let x = 1, 2""")

        Assert.IsTrue(this.ViolationExistsAt(4, 8))

    [<Test>]
    member this.``No tuple instantiation violation for cons operator``() =
        this.Parse("""let x = "" :: aStringList""")

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``Autofix for tuple instantiation without parentheses``() =
        let source = """
module Program

let x = 1, 2"""

        let expected = """
module Program

let x = (1, 2)"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyAutoFix source)

    [<Test>]
    member this.``No violation for tuple instantiation with parentheses``() =
        this.Parse("""
module Program

let x = (1, 2)""")

        Assert.IsTrue(this.NoViolationsExist)
