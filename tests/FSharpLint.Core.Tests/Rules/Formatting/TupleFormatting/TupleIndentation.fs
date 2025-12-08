module FSharpLint.Core.Tests.Rules.Formatting.TupleIndentation

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFormattingTupleIndentation() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(TupleIndentation.rule)

        [<Test>]
    member this.``Violation for tuple with newlines and inconsistent indentation``() =
        this.Parse("""
module Program

let x = (
    1,
        2,
        3)""")

        Assert.IsTrue(this.ViolationExistsAt(5, 4))

    [<Test>]
    member this.``No violation for tuple with newlines and consistent indentation``() =
        this.Parse("""
module Program

let x = (
    1,
    2,
    3)""")

        Assert.IsTrue(this.NoViolationsExist)