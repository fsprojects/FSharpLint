module FSharpLint.Core.Tests.Rules.Conventions.CSharpFriendlyAsyncOverload

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsCSharpFriendlyAsyncOverload() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(CSharpFriendlyAsyncOverload.rule)

    [<Test>]
    member this.``async function must suggest friendly implementation``() =
        this.Parse("""
module Foo =
    let Bar(): Async<unit> =
        Async.Sleep 5""")

        Assert.IsTrue(this.ErrorExistsAt(3, 8))

    [<Test>]
    member this.``async function with friendly implementation must not have errors``() =
        this.Parse("""
module Foo =
    let Bar(): Async<unit> =
        Async.Sleep 5
    let BarAsync(): Task<unit> =
        Bar() |> Async.StartAsTask""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``non async function must not create warnings``() =
        this.Parse("""
module Foo =
    let Bar() =
        ()""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``async function must not have errors when not delcared immediately following the parent function``() =
        this.Parse("""
module Foo =
    let Bar(): Async<unit> =
        Async.Sleep 5
    let RandomFunction() =
        ()
    let BarAsync(): Task<unit> =
        Bar() |> Async.StartAsTask""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``multiple async functions must have errors``() =
        this.Parse("""
module Foo =
    let Bar(): Async<unit> =
        Async.Sleep 5
    let RandomFunction() =
        ()
    let BarAsync(): Task<unit> =
        Bar() |> Async.StartAsTask
    let Foo(): Async<unit> =
        Async.Sleep 10""")

        Assert.IsTrue(this.ErrorExistsAt(9, 8))

    [<Test>]
    member this.``multiple async functions must not have errors``() =
        this.Parse("""
module Foo =
    let Bar(): Async<unit> =
        Async.Sleep 5
    let RandomFunction() =
        ()
    let BarAsync(): Task<unit> =
        Bar() |> Async.StartAsTask
    let Foo(): Async<unit> =
        Async.Sleep 10
    let FooAsync(): Task<unit> =
        Foo() |> Async.StartAsTask""")

        this.AssertNoWarnings()
