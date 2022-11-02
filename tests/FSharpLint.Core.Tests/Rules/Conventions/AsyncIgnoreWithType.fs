module FSharpLint.Core.Tests.Rules.Conventions.AsyncIgnoreWithType

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsAsyncIgnoreWithType() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AsyncIgnoreWithType.rule)

    [<Test>]
    member this.AsyncIgnoreWithoutType() =
        this.Parse("""
namespace Program

module X = 
    let f x = 
        do! x() |> Async.Ignore""")
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AsyncIgnoreWithType() =
        this.Parse("""
namespace Program

module X = 
    let f x = 
        do! x() |> Async.Ignore<int>""")
        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.AsyncIgnoreComplexWithoutType() =
        this.Parse("""
namespace Program

module X = 
    let f x = 
        do! 
            x() 
            |> UnwrapResult
            |> Async.AwaitTask
            |> Async.Ignore""")
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AsyncIgnoreComplexWithtType() =
        this.Parse("""
namespace Program

module X = 
    let f x = 
        do! 
            x() 
            |> UnwrapResult
            |> Async.AwaitTask
            |> Async.Ignore<int>""")
        Assert.IsTrue this.NoErrorsExist
