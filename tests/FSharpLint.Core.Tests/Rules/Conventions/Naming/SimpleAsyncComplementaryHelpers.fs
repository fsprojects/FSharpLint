module FSharpLint.Core.Tests.Rules.Conventions.SimpleAsyncComplementaryHelpers

open NUnit.Framework

open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestSimpleAsyncComplementaryHelpers() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(SimpleAsyncComplementaryHelpers.rule)

    [<Test>]
    member this.``Function AsyncBar should give violations offering creation of BarAsync``() =
        this.Parse """
module Foo =
    let AsyncBar(): Async<int> =
        async { return 0 }
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("BarAsync(): Task<int>", this.ErrorMsg)
        StringAssert.Contains("Async.StartAsTask(AsyncBar())", this.ErrorMsg)

    [<Test>]
    member this.``Non-public functions that return Async should not give violations``() =
        this.Parse """
module Foo =
    let internal AsyncBar(): Async<int> =
        async { return 0 }
    let private AsyncBaz(): Async<int> =
        async { return 0 }
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Functions that comply with conventions should not give violations``() =
        this.Parse """
module Foo =
    let AsyncBar(): Async<int> =
        async { return 0 }
    let BarAsync(): Task<int> =
        Async.StartAsTask(AsyncBar())
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Function BarAsync should give violations offering creation of AsyncBar``() =
        this.Parse """
module Foo =
    let BarAsync(): Task<int> =
        Task.FromResult(1)
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("AsyncBar(): Async<int>", this.ErrorMsg)
        StringAssert.Contains("async { return Async.AwaitTask (BarAsync()) }", this.ErrorMsg)

    [<Test>]
    member this.``Non-public functions that return Task should not give violations``() =
        this.Parse """
module Foo =
    let internal BarAsync(): Task<int> =
        Task.FromResult(1)
    let private BazAsync(): Task<int> =
        Task.FromResult(1)
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Functions that comply with conventions (async one first) should not give violations``() =
        this.Parse """
module Foo =
    let AsyncBar(): Async<int> =
        async { return 0 }
    let BarAsync(): Task<int> =
        Async.StartAsTask(AsyncBar())
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Functions that don't follow naming conventions should not give violations``() =
        this.Parse """
module Foo =
    let Bar(): Async<int> =
        async { return 0 }
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Function AsyncBar that returns Async<unit> should give violations offering creation of BarAsync``() =
        this.Parse """
module Foo =
    let AsyncBar(): Async<unit> =
        Async.Sleep 5.0
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("BarAsync(): Task", this.ErrorMsg)
        StringAssert.Contains("Async.StartAsTask(AsyncBar())", this.ErrorMsg)

    [<Test>]
    member this.``Functions that return Async<unit> and don't follow naming conventions should not give violations``() =
        this.Parse """
module Foo =
    let Bar(): Async<unit> =
        Async.Sleep 5.0
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Function parameters must be preserved in offered solution``() =
        this.Parse """
module Foo =
    let AsyncBar(foo: int): Async<int> =
        async { return 0 }
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("BarAsync(foo: int): Task<int>", this.ErrorMsg)
        StringAssert.Contains("Async.StartAsTask(AsyncBar foo)", this.ErrorMsg)
