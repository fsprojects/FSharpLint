module FSharpLint.Core.Tests.Rules.Conventions.AsynchronousFunctionNames

open NUnit.Framework

open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestAsynchronousFunctionNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AsynchronousFunctionNames.rule)

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
