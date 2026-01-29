module FSharpLint.Core.Tests.Rules.Conventions.FavourNamedMembers

open NUnit.Framework

open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFavourNamedMembers() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourNamedMembers.rule)

    [<Test>]
    member this.``Should produce error for DU with unnamed fields``() =
        this.Parse """
type Foo =
    | Bar of int * int
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for DU with unnamed fields in some cases``() =
        this.Parse """
type Foo =
    | Bar of int * int
    | Baz of baz: string
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for DU with some unnamed fields in on of cases``() =
        this.Parse """
type Foo =
    | Bar of bar: int * int
    | Baz of baz: string
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should not produce error for DU with all named fields``() =
        this.Parse """
type Foo =
    | Bar of bar: int * bar2: int
    | Baz of baz: string
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should not produce error for DU without fields``() =
        this.Parse """
type Foo =
    | Bar
    | Baz
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should not produce error for DU with one case with one field``() =
        this.Parse """
type Foo = | Foo of int
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should produce error for match without named fields``() =
        this.Parse """
type Data =
    | OnePart of num: int
    | TwoPart of num1: int * num2: int

let examineData data =
    match data with
    | OnePart p1 -> p1
    | TwoPart (p1, p2) -> p1 + p2
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should not produce error for match with named fields``() =
        this.Parse """
type Data =
    | OnePart of num: int
    | TwoPart of num1: int * num2: int

let examineData data =
    match data with
    | OnePart(part1=p1) -> p1
    | TwoPart(part1=p1; part2=p2) -> p1 + p2
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should not produce error for match without named fields if DU definition has no named fields``() =
        this.Parse """
type Data =
    | OnePart of int
    | TwoPart of int * int

let examineData data =
    match data with
    | OnePart p1 -> p1
    | TwoPart (p1, p2) -> p1 + p2
"""

        Assert.IsFalse (this.ErrorExistsOnLine 8)
        Assert.IsFalse (this.ErrorExistsOnLine 9)

    [<Test>]
    member this.``Should not produce error for match with wildcard``() =
        this.Parse """
type Data =
    | OnePart of num: int
    | TwoPart of num1: int * num2: int

let examineData data =
    match data with
    | OnePart(_) -> 0
    | TwoPart (_, _) -> 1
"""

        Assert.IsFalse (this.ErrorExistsOnLine 8)
        Assert.IsFalse (this.ErrorExistsOnLine 9)
