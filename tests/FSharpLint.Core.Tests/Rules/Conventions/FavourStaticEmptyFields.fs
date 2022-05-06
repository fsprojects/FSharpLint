module FSharpLint.Core.Tests.Rules.Conventions.FavourStaticEmptyFields

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsFavourStaticEmptyFields() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourStaticEmptyFields.rule)

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError_1() =
        this.Parse "let foo = \"\""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError_2() =
        this.Parse "System.Console.WriteLine \"\""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError_3() =
        this.Parse "let aList = []"

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError_4() =
        this.Parse "let aList = [ ]"

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError_5() =
        this.Parse "System.Console.WriteLine([].Length)"

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError_6() =
        this.Parse """
let foo a =
    if a = 0 then
        "0"
    else 
        "" """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError_7() =
        this.Parse "let anArray = [||]"

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError_8() =
        this.Parse "let anArray = [| |]"

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError_9() =
        this.Parse "System.Console.WriteLine([||].Length)"

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError_1() =
        this.Parse "let bar = String.Empty"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError_2() =
        this.Parse "let foo = \"My Name\""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError_3() =
        this.Parse "System.Console.WriteLine System.String.Empty"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError_4() =
        this.Parse "let aList = List.Empty"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError_5() =
        this.Parse "System.Console.WriteLine(List.Empty.Length.ToString())"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError_6() =
        this.Parse "let anArray = Array.empty"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError_7() =
        this.Parse "System.Console.WriteLine(Array.empty.Length.ToString())"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError_8() =
        this.Parse """
match foo with
| [] -> true
| head::_ -> false"""

        Assert.IsTrue this.NoErrorsExist
