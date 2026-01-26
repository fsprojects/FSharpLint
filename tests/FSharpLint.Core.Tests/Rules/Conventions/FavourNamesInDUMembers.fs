module FSharpLint.Core.Tests.Rules.Conventions.FavourNamesInDUMembers

open NUnit.Framework

open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFavourNamesInDUMembers() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourNamesInDUMembers.rule)

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
