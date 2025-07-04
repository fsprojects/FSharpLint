module FSharpLint.Core.Tests.Rules.Formatting.TupleCommaSpacing

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFormattingTupleCommaSpacing() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(TupleCommaSpacing.rule)

        [<Test>]
    member this.``Error for tuple instantiation without space after comma``() =
        this.Parse("""
module Program

let x = (1,2)""")

        Assert.IsTrue(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.``Quickfix for tuple instantiation without space after comma``() =
        let source = """
module Program

let x = (1,2)"""


        let expected = """
module Program

let x = (1, 2)"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Error for tuple instantiation with two spaces after comma``() =
        this.Parse("""
module Program

let x = (1,  2)""")

        Assert.IsTrue(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.``Quickfix for tuple instantiation with two spaces after comma``() =
        let source = """
module Program

let x = (1,  2)"""


        let expected = """
module Program

let x = (1, 2)"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``No error for tuple instantiation with single space after comma``() =
        this.Parse("""
module Program

let x = (1, 2)""")

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``No error for tuple instantiation with newline after comma``() =
        this.Parse("""
module Program

let x = (
    1, 2,
    3)""")

        Assert.IsTrue(this.NoErrorsExist)
