module FSharpLint.Core.Tests.Rules.Formatting.TypedItemSpacing

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Rules.TypedItemSpacing
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFormattingTypedItemSpaceAfter() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(TypedItemSpacing.rule { Config.TypedItemStyle = TypedItemStyle.SpaceAfter })

    [<Test>]
    member this.``No error for typed pattern with space after colon``() =
        let source = "let (x: int) = 1"
        this.Parse source
        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for typed pattern with no spaces around colon``() =
        let source = "let (x:int) = 1"
        this.Parse source
        Assert.IsTrue(this.ErrorExistsAt(1, 5))

    [<Test>]
    member this.``No error for record field with spaces around colon``() =
        this.Parse "type X = { x : int }"
        Assert.IsTrue(this.ErrorExistsAt(1, 11))

    [<Test>]
    member this.``Fix for typed pattern with no spaces around colon``() =
        let source = "let (x:int) = 1"
        let expected = "let (x: int) = 1"
        this.Parse source
        Assert.AreEqual(expected, this.ApplyFix source)

    [<Test>]
    member this.``Error for typed pattern with spaces around colon``() =
        this.Parse "let (x : int) = 1"
        Assert.IsTrue(this.ErrorExistsAt(1, 5))

    [<Test>]
    member this.``Error for record field with spaces around colon``() =
        this.Parse "type X = { x : int }"
        Assert.IsTrue(this.ErrorExistsAt(1, 11))

    [<Test; Ignore("Test is not passing because UnionCase's field range is incorrect")>]
    member this.``Error for named tuple with spaces around colon``() =
        this.Parse "type X = X of x : int"
        Assert.IsTrue(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.``Fix for typed pattern with spaces around colon``() =
        let source = "let (x : int) = 1"
        let expected = "let (x: int) = 1"
        this.Parse source
        Assert.AreEqual(expected, this.ApplyFix source)

[<TestFixture>]
type TestFormattingTypedItemSpacesAround() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(TypedItemSpacing.rule { Config.TypedItemStyle = TypedItemStyle.SpacesAround })

    [<Test>]
    member this.``No error for typed pattern with spaces around colon``() =
        this.Parse("""
module Program

let (x : int) = 1""")

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for typed pattern with no spaces around colon``() =
        this.Parse("""
module Program

let (x:int) = 1""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.``Fix for typed pattern with spaces around colon``() =
        let source = """
module Program

let (x:int) = 1"""


        let expected = """
module Program

let (x : int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyFix source)

    [<Test>]
    member this.``Error for typed pattern with space after colon``() =
        this.Parse("""
module Program

let (x: int) = 1""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.``Fix for typed pattern with space after colon``() =
        let source = """
module Program

let (x: int) = 1"""


        let expected = """
module Program

let (x : int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyFix source)

[<TestFixture>]
type TestFormattingTypedItemNoSpaces() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(TypedItemSpacing.rule { Config.TypedItemStyle = TypedItemStyle.NoSpaces })

    [<Test>]
    member this.``No error for typed pattern with no spaces around colon``() =
        this.Parse("""
module Program

let (x:int) = 1""")

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for typed pattern with spaces around colon``() =
        this.Parse("""
module Program

let (x : int) = 1""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.``Fix for typed pattern with spaces around colon``() =
        let source = """
module Program

let (x : int) = 1"""


        let expected = """
module Program

let (x:int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyFix source)

    [<Test>]
    member this.``Error for typed pattern with space after colon``() =
        this.Parse("""
module Program

let (x: int) = 1""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.``Fix for typed pattern with space after colon``() =
        let source = """
module Program

let (x: int) = 1"""


        let expected = """
module Program

let (x:int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyFix source)
