module FSharpLint.Core.Tests.Rules.Conventions.LiteralNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

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
let Cat = 5"""

        this.AssertNoWarnings()

    [<Test>]
    member this.LiteralIsCamelCase() =
        this.Parse """
module program

[<Literal>]
let cat = 5"""

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.LiteralIsCamelCaseWithParen() =
        this.Parse """
module program

[<Literal>]
let (cat) = 5"""

        Assert.IsTrue(this.ErrorExistsAt(5, 5))

    [<Test>]
    member this.FullyQualifiedLiteralIsPascalCase() =
        this.Parse """
module Program

[<Microsoft.FSharp.Core.Literal>]
let Cat = 5"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FullyQualifiedLiteralIsCamelCase() =
        this.Parse """
module program

[<Microsoft.FSharp.Core.Literal>]
let cat = 5"""

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

