module FSharpLint.Core.Tests.Rules.Binding.FavourTypedIgnore

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestBindingFavourTypedIgnore() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourTypedIgnore.rule)

    [<Test>]
    member this.``typed ignore causes no violation``() =
        this.Parse
            """
Console.ReadLine() |> ignore<string>
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``typed ignore causes no violation (without pipe)``() =
        this.Parse
            """
 ignore<string>(Console.ReadLine())
 """

        this.AssertNoViolations()

    [<Test>]
    member this.``untyped ignore causes violation``() =
        this.Parse
            """
Console.ReadLine()
|> ignore
"""

        Assert.IsTrue(this.ViolationsExist)
        Assert.IsTrue(this.ViolationExistsAt(2, 0))

    [<Test>]
    member this.``untyped ignore causes violation (without pipe)``() =
        this.Parse
            """
ignore(
    Console.ReadLine()
)
"""

        Assert.IsTrue(this.ViolationsExist)
        Assert.IsTrue(this.ViolationExistsAt(2, 0))
