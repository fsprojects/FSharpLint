module FSharpLint.Core.Tests.Rules.Conventions.FavourSingleton

open System
open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsFavourSingleton() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourSingleton.rule)

    [<Test>]
    member this.ListWithManyItemsShouldNotProduceError() =
        this.Parse """
let foo = [ 10; 20 ]"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ListWithASingleConstantShouldProduceError() =
        this.Parse """
let foo = [ 10 ]"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(2, 12))

    [<Test>]
    member this.ListWithASingleIdentShouldProduceError() =
        this.Parse """
let bar = true
let foo = [ bar ]"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(3, 10))

    [<Test>]
    member this.ListWithMultipleIdentsShouldNotProduceError() =
        this.Parse """
let bar = true
let foo = [ bar; false; true ]"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ListWithManyItemsShouldNotProduceError_Arrays() =
        this.Parse """
let foo = [| 10; 20 |]"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.ListWithASingleConstantShouldProduceError_Arrays() =
        this.Parse """
let foo = [| 10 |]"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(2, 13))

    [<Test>]
    member this.ListWithASingleIdentShouldProduceError_Arrays() =
        this.Parse """
let bar = true
let foo = [| bar |]"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(3, 10))

    [<Test>]
    member this.ListWithMultipleIdentsShouldNotProduceError_Arrays() =
        this.Parse """
let bar = true
let foo = [| bar; false; true |]"""

        this.AssertNoWarnings()

    [<Test>]
    member this.SingletonListWithMatchCaseShouldNotProduceError() =
        this.Parse """
let foo = List.empty
match foo with
| [x] -> printf x
| _ -> printf "baz" """

        this.AssertNoWarnings()

    [<Test>]
    member this.SingletonArrayWithMatchCaseShouldNotProduceError() =
        this.Parse """
let foo = Array.empty
match foo with
| [| x |] -> printf x
| _ -> printf "baz" """

        this.AssertNoWarnings()
