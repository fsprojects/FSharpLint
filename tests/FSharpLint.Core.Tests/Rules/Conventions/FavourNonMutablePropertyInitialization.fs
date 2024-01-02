module FSharpLint.Core.Tests.Rules.Conventions.FavourNonMutablePropertyInitialization

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsFavourNonMutablePropertyInitialization() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourNonMutablePropertyInitialization.rule)

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceError_1() =
        this.Parse """
type SomeClass() =
    let mutable myInternalValue = 1
    // A write-only property.
    member this.MyWriteOnlyProperty with set (value) = myInternalValue <- value

module Program =
    let someFunction() =
        let someInstance = SomeClass()
        someInstance.MyWriteOnlyProperty <- 2"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceError_2() =
        this.Parse """
type SomeClass() =
    let mutable myInternalValue = 1
    // A read-write property.
    member this.MyReadWriteProperty
        with get () = myInternalValue
        and set (value) = myInternalValue <- value

module Program =
    let someFunction() =
        let someInstance = SomeClass()
        someInstance.MyReadWriteProperty <- 2"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceError_3() =
        this.Parse """
open System.Net

module Program =
    let someFunction() =
        // System.Net.Cookie implementation lives in a referenced assembly
        let someInstance = Cookie()
        someInstance.Domain <- "example.com"
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceError_4() =
        this.Parse """
type SomeClass() =
    let mutable myInternalValue = 1
    static member SomeStaticMethod() =
        () // any code goes here
    // A read-write property.
    member this.MyReadWriteProperty
        with get () = myInternalValue
        and set (value) = myInternalValue <- value

module Program =
    let someFunction() =
        let someInstance = SomeClass()
        SomeClass.SomeStaticMethod()
        someInstance.MyReadWriteProperty <- 2"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceError_5() =
        this.Parse """
type SomeClass() =
    let mutable myInternalValue = 1
    // A write-only property.
    member this.MyWriteOnlyProperty with set (value) = myInternalValue <- value

module Program =
    let someFunction() =
        let someInstance = SomeClass()
        someInstance.MyWriteOnlyProperty <- 2
        ()"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldNotProduceError_1() =
        this.Parse """
type SomeClass() =
    let mutable myInternalValue = 1
    // A write-only property.
    member this.MyWriteOnlyProperty with set (value) = myInternalValue <- value

module Program =
    let someFunction() =
        let someInstance = SomeClass(MyWriteOnlyProperty = 2)
        ()"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldNotProduceError_2() =
        this.Parse """
type SomeClass() =
    let mutable myInternalValue = 1
    // A read-write property.
    member this.MyReadWriteProperty
        with get () = myInternalValue
        and set (value) = myInternalValue <- value

module Program =
    let someFunction() =
        let someInstance = SomeClass(MyReadWriteProperty = 2)
        ()"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldNotProduceError_3() =
        this.Parse """
type SomeClass() =
    let mutable myInternalValue = 1
    member this.SomeMethod() =
        () // some code here
    // A read-write property.
    member this.MyReadWriteProperty
        with get () = myInternalValue
        and set (value) = myInternalValue <- value

module Program =
    let someFunction =
        let someInstance = SomeClass()
        someInstance.SomeMethod()
        someInstance.MyReadWriteProperty <- 2"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldNotProduceError_4() =
        this.Parse """
module SomeModule =
    let mutable myInternalValue = 1

module Program =
    let someFunction() =
        SomeModule.myInternalValue <- 2"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldNotProduceError_5() =
        this.Parse """
open System.Net

module Program =
    let someFunction() =
        // System.Net.Cookie implementation lives in a referenced assembly
        let someInstance = Cookie(Domain = "example.com")
        ()"""

        Assert.IsTrue this.NoErrorsExist
