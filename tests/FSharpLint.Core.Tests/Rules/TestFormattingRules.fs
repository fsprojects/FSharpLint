module TestFormattingRules

open NUnit.Framework
open FSharpLint.Rules.Formatting
open FSharpLint.Framework.Configuration

let config = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.ofList 
                [ ("TypedItemSpacing", ruleEnabled) ]
              Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]
 
              
[<TestFixture>]
type Testformatting() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

    [<Test>]
    member this.``Error for typed pattern without spaces around colon``() = 
        this.Parse("""
module Program

let (x:int) = 1""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.``Quickfix for typed pattern with missing spaces``() = 
        let source = """
module Program

let (x:int) = 1"""


        let expected = """
module Program

let (x : int) = 1"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)