module FSharpLint.Core.Tests.Rules.Conventions.RedundantNewKeyword

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsRedundantNewKeyword() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(RedundantNewKeyword.rule)

    [<Test>]
    member this.``Violation when new keyword is not required.``() =
        this.Parse """
module Program

let _ = new System.Version()
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 8))

    [<Test>]
    member this.``New keyword not considered unnecassery if used with a constructor of a type which implements IDisposable.``() =
        this.Parse """
module Program

let _ = new System.IO.MemoryStream()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``Quick fix for unnecassery new keyword.``() =
        let source = """
module Program

let _ = new System.Version()
"""

        let expected = """
module Program

let _ = System.Version()
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``new keyword is not required (1).``() =
        this.Parse """
module Program
    let foo =
        new System.Collections.Generic.Dictionary<string, string>() |> ignore
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``new keyword is not required (2).``() =
        this.Parse """
module Program
    let foo =
        new Guid() |> ignore
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``new keyword is not required (3).``() =
        this.Parse """
module Program
    let foo =
        new Int32() |> ignore
"""

        Assert.IsTrue this.ViolationsExist
