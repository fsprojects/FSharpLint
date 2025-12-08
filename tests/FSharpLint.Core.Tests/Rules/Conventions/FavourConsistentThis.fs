module FSharpLint.Core.Tests.Rules.Conventions.FavourConsistentThis

open System
open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsFavourConsistentThis() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourConsistentThis.rule { Symbol = "this" })

    [<Test>]
    member this.ConsistentThisShouldNotProduceViolation() =
        this.Parse """
type Foo =
    { Bar : Baz }
    member this.FooBar = 
        ()
    member this.FooBarBaz x =
        failwith "foobarbaz"
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.InconsistentSelfShouldProduceViolation1() =
        this.Parse """
type Foo =
    { Bar : Baz }
    member self.FooBar = 
        ()
    member this.FooBarBaz x =
        failwith "foobarbaz"
"""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue(this.ViolationExistsAt(4, 11))

    [<Test>]
    member this.InconsistentSelfShouldProduceViolation2() =
        this.Parse """
type Foo =
    { Bar : Baz }
    member this.FooBar = 
        ()
    member self.FooBarBaz x =
        failwith "foobarbaz"
"""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue(this.ViolationExistsAt(6, 11))

    [<Test>]
    member this.InconsistentSelfShouldProduceViolation3() =
        this.Parse """
type CustomerName(firstName, middleInitial, lastName) =
    member this.FirstName = firstName
    member self.MiddleInitial = middleInitial
    member this.LastName = lastName
"""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue(this.ViolationExistsAt(4, 11))

    [<Test>]
    member this.StaticMethodShouldNotProduceViolation() =
        this.Parse """
type Connection() =
    static member Connect() =
        printfn "New World!"
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.LetShouldNotProduceViolation() =
        this.Parse """
type Connection() =
    let tryGetConnectionRequest() =
        printfn "New World!"
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.UnderScoreMethodShouldNotProduceViolation() =
        this.Parse """
type Connection() =
    member _.AcceptClient() =
        printfn "New World!"
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.AttributeShouldNotProduceViolation() =
        this.Parse """
module Command =
    [<Literal>]
    let AuthChallenge = 130uy
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.NewShouldNotProduceViolation() =
        this.Parse """
type TorMessageDigest(isSha256: bool) =
    new() = TorMessageDigest false
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.InConsistentSelfShouldProduceViolationAndSuggestedFix() =
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

        Assert.IsTrue this.ViolationsExist
        
        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.InConsistentSelfShouldProduceViolationAndSuggestedFix2() =
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

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue(this.ViolationExistsAt(6, 11))

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.InConsistentSelfShouldProduceViolationAndSuggestedFix3() =
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

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue(this.ViolationExistsAt(4, 11))

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)
