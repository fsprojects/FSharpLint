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

    [<Test>]
    member this.StaticMethodShouldNotProduceError() =
        this.Parse """
type Connection() =
    static member Connect() =
        printfn "New World!" """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.LetShouldNotProduceError() =
        this.Parse """
type Connection() =
    let tryGetConnectionRequest() =
        printfn "New World!" """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.UnderScoreMethodShouldNotProduceError() =
        this.Parse """
type Connection() =
    member __.AcceptClient() =
        printfn "New World!" """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.AttributeShouldNotProduceError() =
        this.Parse """
module Command =
    [<Literal>]
    let AuthChallenge = 130uy """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.NewShouldNotProduceError() =
        this.Parse """
type TorMessageDigest(isSha256: bool) =
    new() = TorMessageDigest false """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.InConsistentSelfShouldProduceErrorSuggestedFix() =
        let source = """
type Foo =
    { Bar : Baz }
    member self.FooBar = 
        ()
    member this.FooBarBaz x =
        failwith "foobarbaz" """

        let expected = """
type Foo =
    { Bar : Baz }
    member this.FooBar = 
        ()
    member this.FooBarBaz x =
        failwith "foobarbaz" """

        this.Parse source

        Assert.IsTrue this.ErrorsExist
        
        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.InConsistentSelfShouldProduceErrorSuggestedFix_2() =
        let source = """
type Foo =
    { Bar : Baz }
    member this.FooBar = 
        ()
    member self.FooBarBaz x =
        failwith "foobarbaz" """

        let expected = """
type Foo =
    { Bar : Baz }
    member this.FooBar = 
        ()
    member this.FooBarBaz x =
        failwith "foobarbaz" """

        this.Parse source

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(6, 11))

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.InConsistentSelfShouldProduceErrorSuggestedFix_3() =
        let source = """
type CustomerName(firstName, middleInitial, lastName) =
    member this.FirstName = firstName
    member self.MiddleInitial = middleInitial
    member this.LastName = lastName """
        let expected = """
type CustomerName(firstName, middleInitial, lastName) =
    member this.FirstName = firstName
    member this.MiddleInitial = middleInitial
    member this.LastName = lastName """

        this.Parse source

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(4, 11))

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)
