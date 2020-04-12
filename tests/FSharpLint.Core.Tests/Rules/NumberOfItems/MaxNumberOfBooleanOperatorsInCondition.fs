module FSharpLint.Core.Tests.Rules.NumberOfItems.MaxNumberOfBooleanOperatorsInCondition

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestMaxNumberOfBooleanOperatorsInCondition() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxNumberOfBooleanOperatorsInCondition.rule { MaxItems = 4 })

    [<Test>]
    member this.FourBooleanOperators() =
        this.Parse """
module Program

if not true && (false && false) || true then
    ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FiveBooleanOperators() =
        this.Parse """
module Program

if not true && (false && false) || true (&&) (false) then
    ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 3))
