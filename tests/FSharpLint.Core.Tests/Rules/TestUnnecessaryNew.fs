module TestUnnecessaryNew

open NUnit.Framework
open FSharpLint.Rules.UnnecessaryNew
open FSharpLint.Framework.Configuration

let config = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, { Rules = Map.empty; Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]
              
[<TestFixture>]
type TestUnnecessaryNew() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

    [<Test>]
    member this.``Lint gives suggestion when new keyword is not required.``() = 
        this.Parse("""
module Program

let _ = new System.Version()""", checkInput = true)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.``New keyword not considered unnecassery if used with a constructor of a type which implements IDisposable.``() = 
        this.Parse("""
module Program

let _ = new System.IO.MemoryStream()""", checkInput = true)

        this.AssertNoWarnings()