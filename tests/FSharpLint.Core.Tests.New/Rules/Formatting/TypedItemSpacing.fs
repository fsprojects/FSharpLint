module FSharpLint.Core.Tests.Rules.Formatting.TypedItemSpacing

open NUnit.Framework
open FSharpLint.Rules.TypedItemSpacing
open FSharpLint.Application.ConfigurationManager

let config (style : TypedItemStyle) =
    { ignoreFiles = [||]; Configuration.formatting = Some { typedItemSpacing = Some { Config.typedItemStyle = style} } }
[<TestFixture>]
type TestFormattingTypedItemSpaceAfter() =
    inherit TestRuleBase.TestRuleBase(config TypedItemStyle.SpaceAfter)
    
    [<Test>]
    member this.``No error for typed pattern with space after colon``() = 
        this.Parse("""
module Program

let (x: int) = 1""")

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for typed pattern with no spaces around colon``() = 
        this.Parse("""
module Program

let (x:int) = 1""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.``Quickfix for typed pattern with no spaces around colon``() = 
        let source = """
module Program

let (x:int) = 1"""


        let expected = """
module Program

let (x: int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Error for typed pattern with spaces around colon``() = 
        this.Parse("""
module Program

let (x : int) = 1""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.``Quickfix for typed pattern with spaces around colon``() = 
        let source = """
module Program

let (x : int) = 1"""


        let expected = """
module Program

let (x: int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)


[<TestFixture>]
type TestFormattingTypedItemSpacesAround() =
    inherit TestRuleBase.TestRuleBase(config TypedItemStyle.SpacesAround)

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
    member this.``Quickfix for typed pattern with spaces around colon``() = 
        let source = """
module Program

let (x:int) = 1"""


        let expected = """
module Program

let (x : int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Error for typed pattern with space after colon``() = 
        this.Parse("""
module Program

let (x: int) = 1""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.``Quickfix for typed pattern with space after colon``() = 
        let source = """
module Program

let (x: int) = 1"""


        let expected = """
module Program

let (x : int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)
              
[<TestFixture>]
type TestFormattingTypedItemNoSpaces() =
    inherit TestRuleBase.TestRuleBase(config TypedItemStyle.NoSpaces)

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
    member this.``Quickfix for typed pattern with spaces around colon``() = 
        let source = """
module Program

let (x : int) = 1"""


        let expected = """
module Program

let (x:int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Error for typed pattern with space after colon``() = 
        this.Parse("""
module Program

let (x: int) = 1""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.``Quickfix for typed pattern with space after colon``() = 
        let source = """
module Program

let (x: int) = 1"""


        let expected = """
module Program

let (x:int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)
