module FSharpLint.Core.Tests.Rules.Conventions.AvoidMisleadingRecursiveKeywordInNonRecursiveFuncs

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsAvoidMisleadingRecursiveKeywordInNonRecursiveFuncs() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidMisleadingRecursiveKeywordInNonRecursiveFuncs.rule)

    [<Test>]
    member this.AvoidMisleadingRecursiveKeywordInNonRecursiveFuncsShouldNotProduceError() =
        this.Parse """
let rec Foo () =
    if someParam then
        Foo()
    else
        ()"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.AvoidMisleadingRecursiveKeywordInNonRecursiveFuncsShouldProduceError() =
        this.Parse """
let rec Foo someParam =
    ()"""

        Assert.IsTrue this.ErrorsExist
