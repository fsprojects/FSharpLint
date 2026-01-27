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
