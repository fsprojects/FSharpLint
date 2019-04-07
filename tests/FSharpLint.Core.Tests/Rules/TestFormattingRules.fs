module TestFormattingRules

open NUnit.Framework
open FSharpLint.Rules.Formatting
open FSharpLint.Framework.Configuration

let typedItemSpacingConfig style =
    { 
        Settings = Map.ofList
            [ 
                ("Enabled", Enabled(true))
                ("TypedItemStyle", TypedItemStyle(style))
            ] 
    }

let config typedItemSpacingStyle = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.ofList 
                [ ("TypedItemSpacing", typedItemSpacingConfig typedItemSpacingStyle)
                  ("TupleCommaSpacing", ruleEnabled)
                  ("TupleIndentation", ruleEnabled)
                  ("TupleParentheses", ruleEnabled)
                  ("TypePrefixing", ruleEnabled)
                  ("ModuleDeclSpacing", ruleEnabled)
                  ("ClassMemberSpacing", ruleEnabled)
                  ("UnionDefinitionIndentation", ruleEnabled)
                  ("PatternMatchClausesOnNewLine", ruleEnabled)
                  ("PatternMatchOrClausesOnNewLine", ruleEnabled)
                  ("PatternMatchClauseIndentation", ruleEnabled)
                  ("PatternMatchExpressionIndentation", ruleEnabled) ]
              Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]

[<TestFixture>]
type TestFormatting() =
    inherit TestRuleBase.TestRuleBase(analyser, config TypedItemStyle.NoSpaces)                

    [<Test>]
    member this.``Error for union definition with multiple cases on same line``() =
        this.Parse"""
module Program

type T = T1 of int | T2 of int
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 9))

    [<Test>]
    member this.``Error for union defintion cases at same level as 'type' keyword``() =
        this.Parse"""
module Program

type T = 
| T1 of int 
| T2 of int
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.``Error for union defintion cases with inconsistent spacing``() =
        this.Parse"""
module Program

type T = 
    | T1 of int 
        | T2 of int
"""

        Assert.IsTrue(this.ErrorExistsAt(6, 10))

    [<Test>]
    member this.``No error for correctly indented union defintion cases``() =
        this.Parse"""
module Program

type T = 
    | T1 of int 
    | T2 of int
"""

        Assert.IsTrue(this.NoErrorsExist)
        
    [<Test>]
    member this.``No error for correctly indented union defintion cases with attribute``() =
        this.Parse"""
module Program

type Option =
    | [<CompiledName("is-normal")>] IsNormal
    | CustomClass of string
"""

        Assert.IsTrue(this.NoErrorsExist)       
        
    [<Test>]
    member this.``No error for correctly indented union defintion cases with attribute and strange spacing``() =
        this.Parse"""
module Program

type Option =
    | [< CompiledName("is-normal")>] IsNormal
    | CustomClass of string
"""

        Assert.IsTrue(this.NoErrorsExist)       