module TestNamespacesRules

open NUnit.Framework
open FSharpLint.Rules.Namespaces
open FSharpLint.Framework.Configuration

let config = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.ofList 
                [ ("TopLevelNamespace", ruleEnabled) ]
              Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]
             
 
[<TestFixture>]
type TestNamespaces() =
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