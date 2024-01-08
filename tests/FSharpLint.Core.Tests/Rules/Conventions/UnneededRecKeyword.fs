module FSharpLint.Core.Tests.Rules.Conventions.UnneededRecKeyword

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsUnneededRecKeyword() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UnneededRecKeyword.rule)

    [<Test>]
    member this.UnneededRecKeywordShouldNotProduceError() =
        this.Parse """
let rec Foo () =
    if someParam then
        Foo()
    else
        ()"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.UnneededRecKeywordShouldProduceError_1() =
        this.Parse """
let rec Foo someParam =
    ()"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.UnneededRecKeywordShouldProduceError_2() =
        this.Parse """
let rec Foo someParam =
    ()

[<EntryPoint>]
let main args =
    let Foo () =
        ()

    Foo()
    0"""

        Assert.IsTrue this.ErrorsExist
