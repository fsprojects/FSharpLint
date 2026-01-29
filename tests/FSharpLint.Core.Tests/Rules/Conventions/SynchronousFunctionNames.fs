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
