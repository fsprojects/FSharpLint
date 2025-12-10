module FSharpLint.Core.Tests.Rules.Binding.FavourAsKeyword

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestBindingFavourAsKeyword() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourAsKeyword.rule)

    [<Test>]
    member this.FavourAsKeywordShouldAutoFix() =
        let source = """
module Program

match "" with
| bar when bar = "baz" -> ()
"""

        this.Parse(source)

        let expected = """
module Program

match "" with
| "baz" as bar -> ()
"""

        Assert.AreEqual(expected, this.ApplyAutoFix source)


    [<Test>]
    member this.FavourAsKeywordShouldProduceViolation() =
        this.Parse """
module Program

match "" with
| bar when bar = "baz" -> ()
"""

        this.AssertViolationWithMessageExists("Prefer using the 'as' pattern to match a constant and bind it to a variable.")


    [<Test>]
    member this.FavourAsKeywordShouldNotProduceViolation() =
        this.Parse """
module Program

match "" with
| "baz" as bar -> ()
"""

        Assert.IsTrue(this.NoViolationsExist)

