module FSharpLint.Core.Tests.Rules.Smells.InterpolatedStringWithNoSubstitution

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestSmellsInterpolatedStringWithNoSubstitution() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(InterpolatedStringWithNoSubstitution.rule)

    [<Test>]
    member this.``Interpolated string with no substitutions must generate error``() =
        this.Parse
            """let someString = "foo"
Console.WriteLine $"someString"
"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(2, 18))

    [<Test>]
    member this.``Interpolated string with a substitutions must not generate error``() =
        this.Parse
            """let someString = "foo"
Console.WriteLine $"{someString}"
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``Using sprintf with no substitutions must generate error``() =
        this.Parse
            """let someString = "foo"
Console.WriteLine (sprintf "someString")
"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(2, 19))

    [<Test>]
    member this.``Using sprintf with substitutions must not generate error``() =
        this.Parse
            """let someString = "foo"
sprintf "someString = %s" someString |> Console.WriteLine
someString |> sprintf "someString = %s" |> Console.WriteLine
"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Using curried sprintf must not generate error``() =
        this.Parse
            """let n = 10
Seq.map (sprintf "| %d -> ()") [| 1..len-1 |]
|> ignore
"""

        this.AssertNoWarnings()
