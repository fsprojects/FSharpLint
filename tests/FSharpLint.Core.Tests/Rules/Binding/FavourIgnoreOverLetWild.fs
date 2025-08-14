module FSharpLint.Core.Tests.Rules.Binding.FavourIgnoreOverLetWild

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestBindingFavourIgnoreOverLetWild() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourIgnoreOverLetWild.rule)

    [<Test>]
    member this.LetWildcardUnitValue() =
        this.Parse """
module Program

let _ = ()
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetWildcardMultilaneStatementOfUnit() =
        this.Parse """
module Program

let (_) =
    let x = 4 + 4
    ()
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetWildCardInParanUnitValue() =
        this.Parse """
module Program

let ((((_)))) = List.iter (fun x -> ()) []
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetNonWildcardUnitValue() =
        this.Parse """
module Program

let a = List.iter (fun x -> ()) []
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LetWildcardUnitValueFix() =
        let source = """
module Program

let _ = ()"""
        let expected = """
module Program

(()) |> ignore"""
        this.Parse source

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

        let result = this.ApplyFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.LetWildCardInParanUnitValueFix() =
        let source = """
module Program

let ((((_)))) = List.iter (fun x -> ()) []"""

        let expected = """
module Program

(List.iter (fun x -> ()) []) |> ignore"""

        this.Parse source

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

        let result = this.ApplyFix source

        Assert.AreEqual(expected, result)
