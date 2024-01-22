module FSharpLint.Core.Tests.Rules.Conventions.FavourStaticEmptyFields

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsFavourStaticEmptyFields() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourStaticEmptyFields.rule)

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError1() =
        this.Parse "let foo = \"\""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue (this.ErrorMsg.Contains "String.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError2() =
        this.Parse "System.Console.WriteLine \"\""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue (this.ErrorMsg.Contains "String.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError3() =
        this.Parse "let aList = []"

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue (this.ErrorMsg.Contains "List.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError4() =
        this.Parse "let aList = [ ]"

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue (this.ErrorMsg.Contains "List.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError5() =
        this.Parse "System.Console.WriteLine([].Length)"

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue (this.ErrorMsg.Contains "List.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError6() =
        this.Parse """
let foo a =
    if a = 0 then
        "0"
    else 
        ""
"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue (this.ErrorMsg.Contains "String.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError7() =
        this.Parse "let anArray = [||]"

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue (this.ErrorMsg.Contains "Array.empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError8() =
        this.Parse "let anArray = [| |]"

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue (this.ErrorMsg.Contains "Array.empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError9() =
        this.Parse "System.Console.WriteLine([||].Length)"

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue (this.ErrorMsg.Contains "Array.empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceError10() =
        this.Parse """
type Person =
    {
        FirstName: string
        Nicknames: List<string>
    }

{ FirstName = "Foo"; Nicknames = [] } |> ignore"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue (this.ErrorMsg.Contains "List.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError1() =
        this.Parse "let bar = String.Empty"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError2() =
        this.Parse "let foo = \"My Name\""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError3() =
        this.Parse "System.Console.WriteLine System.String.Empty"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError4() =
        this.Parse "let aList = List.Empty"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError5() =
        this.Parse "System.Console.WriteLine(List.Empty.Length.ToString())"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError6() =
        this.Parse "let anArray = Array.empty"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError7() =
        this.Parse "System.Console.WriteLine(Array.empty.Length.ToString())"

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceError8() =
        this.Parse """
match foo with
| [] -> true
| head::_ -> false"""

        Assert.IsTrue this.NoErrorsExist
