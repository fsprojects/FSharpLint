module TestConventionsRules

open NUnit.Framework
open FSharpLint.Rules.Conventions
open FSharpLint.Framework.Configuration

let config = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.ofList 
                [ ("TopLevelNamespace", ruleEnabled)
                  ("RecursiveAsyncFunction", ruleEnabled) ]
              Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]
             
 
[<TestFixture>]
type TestConventions() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

    [<Test>]
    member this.``Error for top-level module``() = 
        this.Parse """
module Program

let x = ()"""

        Assert.IsTrue(this.ErrorExistsAt(2, 0))

    [<Test>]
    member this.``No error for top-level namespace``() = 
        this.Parse """
namespace Program

module Module = 

    let x = ()"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for recursive async function ending in recursive do!``() = 
        this.Parse("""
namespace Program

module X = 
    let rec f x = async {
        let y = x + 1
        do! f y
    }
""", checkInput=true)

        Assert.IsTrue(this.ErrorExistsAt(7, 8))

    [<Test>]
    member this.``No error for recursive async function ending in recursive return!``() = 
        this.Parse("""
namespace Program

module X = 
    let rec f x = async {
        let y = x + 1
        return! f y
    }
""", checkInput=true)

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``No error for recursive async function ending in non-recursive do!``() = 
        this.Parse("""
namespace Program

module X = 
    let rec f x = async {
        let f = (fun _ ->  async.Return ())
        let y = x + 1
        do! f y
    }
""", checkInput=true)

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Quickfix for recursive async function ending in recursive do!``() = 
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

        this.Parse(source, checkInput=true)
        let result = this.ApplyQuickFix source
        Assert.AreEqual(expected, result)