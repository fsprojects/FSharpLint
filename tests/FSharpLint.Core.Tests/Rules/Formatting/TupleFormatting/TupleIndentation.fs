module FSharpLint.Core.Tests.Rules.Formatting.TupleIndentation

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestFormattingTupleIndentation() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(TupleIndentation.rule)

        [<Test>]
    member this.``Error for tuple with newlines and inconsistent indentation``() =
        this.Parse("""
module Program

let x = (
    1,
        2,
        3)""")

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.``No error for tuple with newlines and consistent indentation``() =
        this.Parse("""
module Program

let x = (
    1,
    2,
    3)""")

        Assert.IsTrue(this.NoErrorsExist)