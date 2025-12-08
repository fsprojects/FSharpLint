module FSharpLint.Core.Tests.Rules.Smells.InterpolatedStringWithNoSubstitution

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestSmellsInterpolatedStringWithNoSubstitution() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(InterpolatedStringWithNoSubstitution.rule)

    [<Test>]
    member this.``Interpolated string with no substitutions must generate violation``() =
        this.Parse
            """let someString = "foo"
Console.WriteLine $"someString"
"""

        Assert.IsTrue(this.ViolationsExist)
        Assert.IsTrue(this.ViolationExistsAt(2, 18))

    [<Test>]
    member this.``Interpolated string with a substitutions must not generate violation``() =
        this.Parse
            """let someString = "foo"
Console.WriteLine $"{someString}"
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``Using sprintf with no substitutions must generate violation``() =
        this.Parse
            """let someString = "foo"
Console.WriteLine (sprintf "someString")
"""

        Assert.IsTrue(this.ViolationsExist)
        Assert.IsTrue(this.ViolationExistsAt(2, 19))

    [<Test>]
    member this.``Using sprintf with substitutions must not generate violation``() =
        this.Parse
            """let someString = "foo"
sprintf "someString = %s" someString |> Console.WriteLine
someString |> sprintf "someString = %s" |> Console.WriteLine
"""
        
        this.AssertNoViolations()

    [<Test>]
    member this.``Using curried sprintf must not generate violation``() =
        this.Parse
            """let n = 10
Seq.map (sprintf "| %d -> ()") [| 1..len-1 |]
|> ignore
"""

        this.AssertNoViolations()
