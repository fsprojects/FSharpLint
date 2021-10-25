module FSharpLint.Core.Tests.Rules.Conventions.FavourConsistentThis

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsFavourConsistentThis() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourConsistentThis.rule { Symbol = "this" })

    [<Test>]
    member this.ConsistentThisShouldNotProduceError() =
        this.Parse """
type Foo =
    { Bar : Baz }
    member this.FooBar = 
        ()
    member this.FooBarBaz x =
        failwith "foobarbaz" """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.InConsistentSelfShouldProduceError() =
        this.Parse """
type Foo =
    { Bar : Baz }
    member self.FooBar = 
        ()
    member this.FooBarBaz x =
        failwith "foobarbaz" """

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(4, 11))

    [<Test>]
    member this.InConsistentSelfShouldProduceError_2() =
        this.Parse """
type Foo =
    { Bar : Baz }
    member this.FooBar = 
        ()
    member self.FooBarBaz x =
        failwith "foobarbaz" """

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(6, 11))

    [<Test>]
    member this.InConsistentSelfShouldProduceError_3() =
        this.Parse """
type CustomerName(firstName, middleInitial, lastName) =
    member this.FirstName = firstName
    member self.MiddleInitial = middleInitial
    member this.LastName = lastName """

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(4, 11))

