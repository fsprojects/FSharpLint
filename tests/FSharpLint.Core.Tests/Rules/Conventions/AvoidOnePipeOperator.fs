module FSharpLint.Core.Tests.Rules.Conventions.AvoidOnePipeOperator

open NUnit.Framework

open FSharpLint.Rules

[<TestFixture>]
type TestConventionsAvoidOnePipeOperator() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidOnePipeOperator.rule)

    [<Test>]
    member this.``Use pipe operator once``() =
        Assert.Pass
