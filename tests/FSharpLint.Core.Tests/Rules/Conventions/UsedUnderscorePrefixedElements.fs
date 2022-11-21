module FSharpLint.Core.Tests.Rules.Conventions.UsedUnderscorePrefixedElements

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsUsedUnderscorePrefixedElements() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UsedUnderscorePrefixedElements.rule)

    [<Test>]
    member this.``Lint flags record member(s) with type hints``() =
        this.Parse """
type MyClass() =
    let _random  = new System.Random()
    member val RandomOpt = (_random.Next() |> Some) with get, set """

        Assert.IsTrue this.ErrorsExist

