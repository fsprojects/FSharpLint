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
