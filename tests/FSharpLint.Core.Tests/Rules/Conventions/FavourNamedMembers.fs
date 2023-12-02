module FSharpLint.Core.Tests.Rules.Conventions.FavourNamedMembers

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsFavourNamedMembers() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourNamedMembers.rule)

    [<Test>]
    member this.NamedMembersShouldNotProduceError_1() =
        this.Parse """
open System

let examineData x =
    match data with
    | OnePartData(part1=p1) -> p1
    | TwoPartData(part1=p1; part2=p2) -> p1 + p2"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.NamedMembersShouldNotProduceError_2() =
        this.Parse """
type Data =
    | TwoParts of part1: string * part2: string
    | OnePart of part0: string"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.UnnamedMembersSelfShouldProduceError_1() =
        this.Parse """
let examineData x =
    match data with
    | OnePart p1 -> p1
    | TwoParts (p1, p2) -> p1 + p2"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.UnnamedMembersSelfShouldProduceError_2() =
        this.Parse """
type Data =
    | TwoParts of string * string
    | OnePart of string"""

        Assert.IsTrue this.ErrorsExist

