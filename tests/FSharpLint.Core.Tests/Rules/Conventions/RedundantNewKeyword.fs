module FSharpLint.Core.Tests.Rules.Conventions.RedundantNewKeyword

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type internal TestConventionsRedundantNewKeyword() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(RedundantNewKeyword.rule)

    [<Test>]
    member this.``Lint gives suggestion when new keyword is not required.``() =
        this.Parse("""
module Program

let _ = new System.Version()""")

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.``New keyword not considered unnecassery if used with a constructor of a type which implements IDisposable.``() =
        this.Parse("""
module Program

let _ = new System.IO.MemoryStream()""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Quick fix for unnecassery new keyword.``() =
        let source = """
module Program

let _ = new System.Version()"""

        let expected = """
module Program

let _ = System.Version()"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)