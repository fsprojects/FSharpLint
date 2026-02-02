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
        StringAssert.Contains("BarAsync", this.ErrorMsg)
