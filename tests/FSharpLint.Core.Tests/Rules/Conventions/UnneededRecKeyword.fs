module FSharpLint.Core.Tests.Rules.Conventions.UnneededRecKeyword

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsUnneededRecKeyword() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UnneededRecKeyword.rule)

    [<Test>]
    member this.UnneededRecKeywordShouldNotProduceViolation() =
        this.Parse """
let rec Foo () =
    if someParam then
        Foo()
    else
        ()
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.UnneededRecKeywordShouldProduceViolation1() =
        this.Parse """
let rec Foo someParam =
    ()
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.UnneededRecKeywordShouldProduceViolation2() =
        this.Parse """
let rec Foo someParam =
    ()

let Bar () =
    let Foo () =
        ()

    Foo()
    ()
"""

        Assert.IsTrue this.ViolationsExist
