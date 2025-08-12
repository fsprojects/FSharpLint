module FSharpLint.Core.Tests.Rules.Conventions.LiteralNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Core.Tests

let config =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }

[<TestFixture>]
type TestConventionsLiteralNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(LiteralNames.rule config)

    [<Test>]
    member this.LiteralIsPascalCase() =
        this.Parse """
module Program

[<Literal>]
let Cat = 5
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.LiteralIsCamelCase() =
        this.Parse """
module program

[<Literal>]
let cat = 5
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.LiteralIsCamelCaseWithParen() =
        this.Parse """
module program

[<Literal>]
let (cat) = 5
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 5))

    [<Test>]
    member this.FullyQualifiedLiteralIsPascalCase() =
        this.Parse """
module Program

[<Microsoft.FSharp.Core.Literal>]
let Cat = 5
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FullyQualifiedLiteralIsCamelCase() =
        this.Parse """
module program

[<Microsoft.FSharp.Core.Literal>]
let cat = 5
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

let infixConfig =
    { NamingConfig.Naming = Some NamingCase.AllLowercase
      Underscores = Some NamingUnderscores.AllowInfix
      Prefix = None
      Suffix = None }

[<TestFixture>]
type TestConventionsLiteralNamesInfix() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(LiteralNames.rule infixConfig)

    [<Test>]
    member this.LiteralIsInfixUnderscore() =
        this.Parse """
module Program

[<Literal>]
let super_cat = 5
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.LiteralIsUnderscorePrefix() =
        this.Parse """
module Program

[<Literal>]
let _cat = 5
"""

        Assert.IsTrue(this.ErrorExistsOnLine 5)

    [<Test>]
    member this.LiteralIsUnderscoreSuffix() =
        this.Parse """
module Program

[<Literal>]
let cat_ = 5
"""

        Assert.IsTrue(this.ErrorExistsOnLine 5)

    [<Test>]
    member this.LiteralIsNoUnderscore() =
        this.Parse """
module Program

[<Literal>]
let SuperCat = 5
"""

        Assert.IsTrue(this.ErrorExistsOnLine 5)

    [<Test>]
    member this.SuggestedFix() =
        let source = """
module Program

[<Literal>]
let superCat = 5
"""
        let expected = """
module Program

[<Literal>]
let super_cat = 5
"""
        this.Parse source

        Assert.IsTrue(this.ErrorExistsOnLine 5)

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)
