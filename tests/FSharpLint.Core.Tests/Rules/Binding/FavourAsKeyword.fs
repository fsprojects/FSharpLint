module FSharpLint.Core.Tests.Rules.Binding.FavourAsKeyword

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestBindingFavourAsKeyword() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourAsKeyword.rule)

    [<Test>]
    member this.FavourAsKeywordShouldQuickFixAndProduceError() =
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

        Assert.AreEqual(expected, this.ApplyQuickFix source)
        this.AssertErrorWithMessageExists("Prefer using the as pattern to match a constant and bind it to a variable.")


    [<Test>]
    member this.FavourAsKeywordShouldNotProduceError() =
        this.Parse """
module Program

match "" with
| "baz" as bar -> ()
"""

        Assert.IsTrue(this.NoErrorsExist)

