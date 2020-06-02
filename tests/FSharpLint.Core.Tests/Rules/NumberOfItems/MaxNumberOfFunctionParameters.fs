module FSharpLint.Core.Tests.Rules.NumberOfItems.MaxNumberOfFunctionParameters

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type internal TestMaxNumberOfFunctionParameters() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxNumberOfFunctionParameters.rule { MaxItems = 5 })

    [<Test>]
    member this.SixParameters() =
        this.Parse """
module Program

let foo one two three four five six = ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 32))

    [<Test>]
    member this.FiveParameters() =
        this.Parse """
module Program

let foo one two three four five = ()"""

        this.AssertNoWarnings()
