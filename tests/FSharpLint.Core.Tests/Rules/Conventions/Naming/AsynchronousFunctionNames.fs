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
