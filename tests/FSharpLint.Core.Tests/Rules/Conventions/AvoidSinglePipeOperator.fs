module FSharpLint.Core.Tests.Rules.Conventions.AvoidSinglePipeOperator

open NUnit.Framework

open FSharpLint.Rules

[<TestFixture>]
type TestConventionsAvoidSinglePipeOperator() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidSinglePipeOperator.rule)

    [<Test>]
    member this.``Use pipe operator once``() =
        this.Parse """
let someFunc someParam =
    someParam
    |> someOtherFunc
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Use pipe operator twice``() =
        this.Parse """
let someFunc someParam =
    someParam
    |> someOtherFunc
    |> yetAnotherFunc
"""

        Assert.IsFalse this.ErrorsExist

    [<Test>]
    member this.``Use pipe operator once in module``() =
        this.Parse """
module MyModule =
    let someFunc someParam =
        someParam
        |> someOtherFunc
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Use pipe operator twice in module``() =
        this.Parse """
module MyModule =
    let someFunc someParam =
        someParam
        |> someOtherFunc
        |> yetAnotherFunc
"""

        Assert.IsFalse this.ErrorsExist

    [<Test>]
    member this.``Use pipe operator once in type``() =
        this.Parse """
type CustomerName(firstName) =
    member this.someFunc someParam =
        someParam
        |> someOtherFunc
        """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Use pipe operator twice in type``() =
        this.Parse """
type CustomerName(firstName) =
    member this.someFunc someParam =
        someParam
        |> someOtherFunc
        |> yetAnotherFunc
        """

        Assert.IsFalse this.ErrorsExist

    [<Test>]
    member this.``Use pipe operator once to avoid parenthesis``() =
        this.Parse """
let someFunc someParam =
    someOtherFunc1 someParam
    |> someOtherFunc2
"""

        Assert.IsFalse this.ErrorsExist
