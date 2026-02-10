module FSharpLint.Core.Tests.Rules.Conventions.AsynchronousFunctionNames

open NUnit.Framework

open FSharpLint.Rules
open FSharpLint.Core.Tests
open FSharpLint.Rules.Helper.Naming.Asynchronous

[<TestFixture>]
type TestAsynchronousFunctionNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AsynchronousFunctionNames.rule { Mode = AnyPublicAPIs })

    [<Test>]
    member this.``Function returning Async<'T> should give violations offering adding Async prefix``() =
        this.Parse """
module Foo =
    let Bar(): Async<int> =
        async { return 1 }
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("AsyncBar", this.ErrorMsg)

    [<Test>]
    member this.``Function returning Task<'T> should give violations offering adding Async suffix``() =
        this.Parse """
module Foo =
    let Bar(): Task<int> =
        null
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("BarAsync", this.ErrorMsg)

    [<Test>]
    member this.``Function returning Task should give violations offering adding Async suffix``() =
        this.Parse """
module Foo =
    let Bar(): Task =
        null
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("BarAsync", this.ErrorMsg)

    [<Test>]
    member this.``Non-public functions should give no violations``() =
        this.Parse """
module Foo =
    let private Bar1(): Async<int> =
        async { return 1 }
    let internal BarBaz1(): Async<int> =
        async { return 1 }
    let private Bar2(): Task<int> =
        null
    let internal BarBaz2(): Task =
        null
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Nested functions should give no violations``() =
        this.Parse """
module Foo =
    let Foo() =
        let Bar(): Async<int> =
            async { return 1 }
        ()
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Nested functions inside methods and type's functions should give no violations``() =
        this.Parse """
type Foo() =
    let foo () =
        let Bar(): Async<int> =
            async { return 1 }
        ()

    member this.FooBar() =
        let Baz(): Async<int> =
            async { return 1 }
        ()
"""

        Assert.IsTrue this.NoErrorsExist
    
    [<Test>]
    member this.``Method returning Async<'T> should give violations offering adding Async prefix``() =
        this.Parse """
type Foo() =
    member this.Bar(): Async<int> =
        async { return 1 }
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("AsyncBar", this.ErrorMsg)

    [<Test>]
    member this.``Method returning Task<'T> should give violations offering adding Async suffix``() =
        this.Parse """
type Foo() =
    member this.Bar(): Task<int> =
        null
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("BarAsync", this.ErrorMsg)

    [<Test>]
    member this.``Method returning Task should give violations offering adding Async suffix``() =
        this.Parse """
type Foo() =
    member this.Bar(): Task =
        null
"""

        Assert.IsTrue this.ErrorsExist
        StringAssert.Contains("BarAsync", this.ErrorMsg)

    [<Test>]
    member this.``Non-public methods should give no violations``() =
        this.Parse """
type Foo() =
    member private this.Bar1(): Async<int> =
        async { return 1 }
    member internal this.BarBaz1(): Async<int> =
        async { return 1 }
    member private this.Bar2(): Task<int> =
        null
    member internal this.BarBaz2(): Task =
        null
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Functions returning Async or Task with [<Obsolete>] attribute should not give violations``() =
        this.Parse """
module Foo =
    [<Obsolete>]
    let Foo(): Async<int> =
        async { return 1 }

    [<Obsolete>]
    let Bar(): Task<int> =
        null

    [<Obsolete>]
    let Baz(): Task =
        null
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Methods returning Async or Task with [<Obsolete>] attribute should not give violations``() =
        this.Parse """
type Foo() =
    [<Obsolete>]
    member this.Foo(): Async<int> =
        async { return 1 }

    [<Obsolete>]
    member this.Bar(): Task<int> =
        null

    [<Obsolete>]
    member this.Baz(): Task =
        null
"""

        Assert.IsTrue this.NoErrorsExist
