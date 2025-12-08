module FSharpLint.Core.Tests.Rules.Conventions.FavourStaticEmptyFields

open System
open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsFavourStaticEmptyFields() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourStaticEmptyFields.rule)

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation1() =
        this.Parse "let foo = \"\""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "String.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation2() =
        this.Parse "System.Console.WriteLine \"\""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "String.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation3() =
        this.Parse "let aList = []"

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "List.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation4() =
        this.Parse "let aList = [ ]"

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "List.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation5() =
        this.Parse "System.Console.WriteLine([].Length)"

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "List.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation6() =
        this.Parse """
let foo a =
    if a = 0 then
        "0"
    else 
        ""
"""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "String.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation7() =
        this.Parse "let anArray = [||]"

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "Array.empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation8() =
        this.Parse "let anArray = [| |]"

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "Array.empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation9() =
        this.Parse "System.Console.WriteLine([||].Length)"

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "Array.empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation10() =
        this.Parse """
type Person =
    {
        FirstName: string
        Nicknames: List<string>
    }

{ FirstName = "Foo"; Nicknames = [] } |> ignore"""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "List.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation11() =
        this.Parse """
type Person =
    {
        FirstName: string
    }

{ FirstName = "" } |> ignore"""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "String.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldProduceViolation12() =
        this.Parse """
type Person =
    {
        FirstName: string
    }

{ FirstName = fooGetFirstName "" } |> ignore"""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue (this.ViolationMsg.Contains "String.Empty")

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceViolation1() =
        this.Parse "let bar = String.Empty"

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceViolation2() =
        this.Parse "let foo = \"My Name\""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceViolation3() =
        this.Parse "System.Console.WriteLine System.String.Empty"

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceViolation4() =
        this.Parse "let aList = List.Empty"

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceViolation5() =
        this.Parse "System.Console.WriteLine(List.Empty.Length.ToString())"

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceViolation6() =
        this.Parse "let anArray = Array.empty"

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceViolation7() =
        this.Parse "System.Console.WriteLine(Array.empty.Length.ToString())"

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsShouldNotProduceViolation8() =
        this.Parse """
match foo with
| [] -> true
| head::_ -> false"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.FavourStaticEmptyFieldsSuggestedFixForString() =
        let source = "let foo = \"\""

        let expected = "let foo = String.Empty"
        
        this.Parse source

        Assert.IsTrue this.ViolationsExist

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.FavourStaticEmptyFieldsSuggestedFixForList() =
        let source = "let foo = []"

        let expected = "let foo = List.Empty"
        
        this.Parse source

        Assert.IsTrue this.ViolationsExist

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.FavourStaticEmptyFieldsSuggestedFixForArray() =
        let source = "let foo = [||]"

        let expected = "let foo = Array.empty"
        
        this.Parse source

        Assert.IsTrue this.ViolationsExist

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)
