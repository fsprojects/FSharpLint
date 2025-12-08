module FSharpLint.Core.Tests.Rules.Conventions.FavourNonMutablePropertyInitialization

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsFavourNonMutablePropertyInitialization() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourNonMutablePropertyInitialization.rule)

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceViolation1() =
        this.Parse """
type SomeClass() =
    let mutable myInternalValue = 1
    // A write-only property.
    member this.MyWriteOnlyProperty with set (value) = myInternalValue <- value

module Program =
    let someFunction() =
        let someInstance = SomeClass()
        someInstance.MyWriteOnlyProperty <- 2"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceViolation2() =
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

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceViolation3() =
        this.Parse """
open System.Net

module Program =
    let someFunction() =
        // System.Net.Cookie implementation lives in a referenced assembly
        let someInstance = Cookie()
        someInstance.Domain <- "example.com"
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceViolation4() =
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

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceViolation5() =
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

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldProduceViolation6() =
        this.Parse """
type SomeClass() =
    member val SomeProperty1 = 0 with get, set
    member val SomeProperty2 = 10 with get, set

module Program =
    let someFunction =
        let someInstance = SomeClass()
        someInstance.SomeProperty1 <- 2
        someInstance.SomeProperty2 <- 3"""

        Assert.IsTrue <| this.ViolationExistsAt(9, 21)
        Assert.IsTrue <| this.ViolationExistsAt(10, 21)

    [<Test>]
    member this.``FavourNonMutablePropertyInitialization should produce violation in match expression``() =
        this.Parse """
type SomeClass() =
    member val MyWriteOnlyProperty = 0 with set

let someValue =
    match () with
    | _ ->
        let someInstance = SomeClass()
        someInstance.MyWriteOnlyProperty <- 2"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``FavourNonMutablePropertyInitialization should produce violation in lambda function``() =
        this.Parse """
type SomeClass() =
    member val MyWriteOnlyProperty = 0 with set

let someLambda =
    fun x ->
        let someInstance = SomeClass()
        someInstance.MyWriteOnlyProperty <- 2"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``FavourNonMutablePropertyInitialization should produce violation in try-with block``() =
        this.Parse """
type SomeClass() =
    member val MyWriteOnlyProperty = 0 with set

let someValue =
    try
        let someInstance = SomeClass()
        someInstance.MyWriteOnlyProperty <- 2
    with
    | _ -> ()"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``FavourNonMutablePropertyInitialization should produce violation in finally block``() =
        this.Parse """
type SomeClass() =
    member val MyWriteOnlyProperty = 0 with set

let someValue =
    try
        ()
    finally
        let someInstance = SomeClass()
        someInstance.MyWriteOnlyProperty <- 2"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``FavourNonMutablePropertyInitialization should produce violation in computation expression``() =
        this.Parse """
type SomeClass() =
    member val MyWriteOnlyProperty = 0 with set

let someValue =
    async {
        let someInstance = SomeClass()
        someInstance.MyWriteOnlyProperty <- 2
    }"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldNotProduceViolation1() =
        this.Parse """
type SomeClass() =
    let mutable myInternalValue = 1
    // A write-only property.
    member this.MyWriteOnlyProperty with set (value) = myInternalValue <- value

module Program =
    let someFunction() =
        let someInstance = SomeClass(MyWriteOnlyProperty = 2)
        ()"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldNotProduceViolation2() =
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

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldNotProduceViolation3() =
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

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldNotProduceViolation4() =
        this.Parse """
module SomeModule =
    let mutable myInternalValue = 1

module Program =
    let someFunction() =
        SomeModule.myInternalValue <- 2"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourNonMutablePropertyInitializationShouldNotProduceViolation5() =
        this.Parse """
open System.Net

module Program =
    let someFunction() =
        // System.Net.Cookie implementation lives in a referenced assembly
        let someInstance = Cookie(Domain = "example.com")
        ()"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.``FavourNonMutablePropertyInitialization should not produce violation on local variables``() =
        this.Parse """
let someFunc () =
    let mutable current = 23
    current <- 2
    ()"""

        Assert.IsTrue this.NoViolationsExist
