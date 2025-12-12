module FSharpLint.Core.Tests.Rules.Conventions.RecursiveAsyncFunction

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsRecursiveAsyncFunction() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(RecursiveAsyncFunction.rule)

    [<Test>]
    member this.``Violation for recursive async function ending in recursive do!``() = 
        this.Parse """
namespace Program

module X = 
    let rec f x = async {
        let y = x + 1
        do! f y
    }
"""

        Assert.IsTrue(this.ViolationExistsAt(7, 8))

    [<Test>]
    member this.``No violation for recursive async function ending in recursive return!``() = 
        this.Parse """
namespace Program

module X = 
    let rec f x = async {
        let y = x + 1
        return! f y
    }
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for recursive async function ending in non-recursive do!``() = 
        this.Parse """
namespace Program

module X = 
    let rec f x = async {
        let f = (fun _ ->  async.Return ())
        let y = x + 1
        do! f y
    }
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``Autofix for recursive async function ending in recursive do!``() =
        let source = """
namespace Program

module X = 
    let rec f x = async {
        let y = x + 1
        do! f y
    }
"""

        let expected = """
namespace Program

module X = 
    let rec f x = async {
        let y = x + 1
        return! f y
    }
"""

        this.Parse(source)
        let result = this.ApplyAutoFix source
        Assert.AreEqual(expected, result)
