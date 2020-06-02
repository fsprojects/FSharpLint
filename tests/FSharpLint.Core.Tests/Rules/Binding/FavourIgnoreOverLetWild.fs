module FSharpLint.Core.Tests.Rules.Binding.FavourIgnoreOverLetWild

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type internal TestBindingFavourIgnoreOverLetWild() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourIgnoreOverLetWild.rule)

    [<Test>]
    member this.LetWildcardUnitValue() =
        this.Parse """
module Program

let _ = ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetWildcardMultilaneStatementOfUnit() =
        this.Parse """
module Program

let (_) =
  let x = 4 + 4
  ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetWildCardInParanUnitValue() =
        this.Parse """
module Program

let ((((_)))) = List.iter (fun x -> ()) []"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetNonWildcardUnitValue() =
        this.Parse """
module Program

let a = List.iter (fun x -> ()) []"""

        Assert.IsFalse(this.ErrorsExist)

