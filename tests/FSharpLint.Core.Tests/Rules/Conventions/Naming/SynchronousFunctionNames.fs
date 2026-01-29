module FSharpLint.Core.Tests.Rules.Conventions.SynchronousFunctionNames

open NUnit.Framework

open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestSynchronousFunctionNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(SynchronousFunctionNames.rule)

    [<Test>]
    member this.``Non-asynchronous function named Async* should give violations offering removing Async prefix``() =
        this.Parse """
module Foo =
    let AsyncBar(): int =
        1
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("Bar", this.ErrorMsg)

    [<Test>]
    member this.``Non-asynchronous function named *Async should give violations offering removing Async suffix``() =
        this.Parse """
module Foo =
    let BarAsync(): int =
        1
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("Bar", this.ErrorMsg)

    [<Test>]
    member this.``Non-asynchronous nested function named *Async should give violations offering removing Async suffix``() =
        this.Parse """
module Foo =
    let Bar(): int =
        let BazAsync(): int =
            0
        1
"""
        
        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("Baz", this.ErrorMsg)

    [<Test>]
    member this.``Private non-asynchronous function named Async* should give violations offering removing Async suffix``() =
        this.Parse """
module Foo =
    let private AsyncBar(): int =
        1
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("Bar", this.ErrorMsg)

    [<Test>]
    member this.``Private non-asynchronous function named async* should give violations offering removing Async suffix``() =
        this.Parse """
module Foo =
    let private asyncBar(): int =
        1
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("bar", this.ErrorMsg)

    [<Test>]
    member this.``Internal non-asynchronous function named *Async should give violations offering removing Async suffix``() =
        this.Parse """
module Foo =
    let internal AsyncBar(): int =
        1
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("Bar", this.ErrorMsg)

    [<Test>]
    member this.``Async functions with Async prefix should give no violations``() =
        this.Parse """
let AsyncFoo(): Async<int> =
    async { return 1 }
let AsyncBar(): Async<unit> =
    async { return () }
let AsyncBaz(): Async<unit> =
    async { do! Async.Sleep(1000) }
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Functions that return Task with Async suffix should give no violations``() =
        this.Parse """
let FooAsync(): Task =
    null
let BarAsync(): Task<int> =
    null
"""

        Assert.IsTrue this.NoErrorsExist


    [<Test>]
    member this.``Non-asynchronous method named Async* should give violations offering removing Async prefix``() =
        this.Parse """
type Foo() =
    member this.AsyncBar(): int =
        1
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("Bar", this.ErrorMsg)

    [<Test>]
    member this.``Non-asynchronous method named *Async should give violations offering removing Async suffix``() =
        this.Parse """
type Foo() =
    member this.BarAsync(): int =
        1
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("Bar", this.ErrorMsg)

    [<Test>]
    member this.``Private non-asynchronous method named *Async should give violations offering removing Async suffix``() =
        this.Parse """
type Foo() =
    member private this.AsyncBar(): int =
        1
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("Bar", this.ErrorMsg)

    [<Test>]
    member this.``Internal non-asynchronous method named *Async should give violations offering removing Async suffix``() =
        this.Parse """
type Foo() =
    member internal this.AsyncBar(): int =
        1
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("Bar", this.ErrorMsg)

    [<Test>]
    member this.``Async methods with Async prefix should give no violations``() =
        this.Parse """
type Foo() =
    member this.AsyncFoo(): Async<int> =
        async { return 1 }
    member this.AsyncBar(): Async<unit> =
        async { return () }
    member this.AsyncBaz(): Async<unit> =
        async { do! Async.Sleep(1000) }
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Methods that return Task with Async suffix should give no violations``() =
        this.Parse """
type Foo() =
    member this.FooAsync(): Task =
        null
    member this.BarAsync(): Task<int> =
        null
"""

        Assert.IsTrue this.NoErrorsExist
