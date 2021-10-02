module FSharpLint.Core.Tests.Rules.Binding.FavourTypedIgnore

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestBindingFavourTypedIgnore() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourTypedIgnore.rule)

    [<Test>]
    member this.``typed ignore has no error``() =
        this.Parse
            """
Console.ReadLine() |> ignore<string>"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``typed ignore has no error (without pipe)``() =
        this.Parse
            """
 ignore<string>(Console.ReadLine())"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``untyped ignore has errors``() =
        this.Parse
            """
Console.ReadLine()
|> ignore"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(2, 0))

    [<Test>]
    member this.``untyped ignore has errors (without pipe)``() =
        this.Parse
            """
ignore(
    Console.ReadLine()
)"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(2, 0))
