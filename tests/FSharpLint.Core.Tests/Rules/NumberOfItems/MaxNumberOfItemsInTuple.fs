module FSharpLint.Core.Tests.Rules.NumberOfItems.MaxNumberOfItemsInTuple

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestMaxNumberOfItemsInTuple() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxNumberOfItemsInTuple.rule { MaxItems = 5 })

    [<Test>]
    member this.SixTupleItemsExpressionConstructor() =
        this.Parse """
module Program

type Test(a,b,c,d,e,f) =
    member this.One() = ()

let dog = Test(1,2,3,4,5,6)"""

        this.AssertNoWarnings()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/141
    /// Note: we are just disabling all warnings for tuples in function applications
    /// because in a lot of places the user won't have control over the definition
    /// of the function - the definition of a function should be where the lint is warning.
    [<Test>]
    member this.``Tuple with too many items in a functiona application must never issue a warning.``() =
        this.Parse """
foo (1,2,3,4,5,6)"""

        this.AssertNoWarnings()

    [<Test>]
    member this.SixTupleItemsExpressionConstructorWithNew() =
        this.Parse """
module Program

type Test(a,b,c,d,e,f) =
    member this.One() = ()

let dog = new Test(1,2,3,4,5,6)"""

        this.AssertNoWarnings()

    [<Test>]
    member this.SixTupleItemsExpressionCallingMethod() =
        this.Parse """
module Program

type Test() =
    member this.One(a,b,c,d,e,f) = ()

let test = Test()

let dog =
    test.One(1,2,3,4,5,6)"""

        this.AssertNoWarnings()

    [<Test>]
    member this.SixTupleItemsExpression() =
        this.Parse """
module Program

let foo = (1, 2, 3, 4, 5, 6)"""

        Assert.IsTrue(this.ErrorExistsAt(4, 26))

    [<Test>]
    member this.FiveTupleItemsExpression() =
        this.Parse """
module Program

let foo = (1, 2, 3, 4, 5)"""

        this.AssertNoWarnings()