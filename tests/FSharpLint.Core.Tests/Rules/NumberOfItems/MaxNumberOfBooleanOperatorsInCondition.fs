module FSharpLint.Core.Tests.Rules.NumberOfItems.MaxNumberOfBooleanOperatorsInCondition

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestMaxNumberOfBooleanOperatorsInCondition() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxNumberOfBooleanOperatorsInCondition.rule { MaxItems = 4 })

    [<Test>]
    member this.FourBooleanOperators() =
        this.Parse """
module Program

if not true && (false && false) || true then
    ()
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FiveBooleanOperators() =
        this.Parse """
module Program

if not true && (false && false) || true (&&) (false) then
    ()
"""

        Assert.ErrorExistsAt(this, (4, 3))
