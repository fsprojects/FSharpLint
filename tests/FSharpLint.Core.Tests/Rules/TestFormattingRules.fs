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
    member this.``Error for F# List type prefix syntax``() =
        this.Parse """
module Program

type T = list<int>
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 9))

    [<Test>]
    member this.``No error for F# List type postfix syntax``() =
        this.Parse """
module Program

type T = int list
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for F# Option type prefix syntax``() =
        this.Parse """
module Program

type T = Option<int>
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 9))

    [<Test>]
    member this.``No error for F# Option type postfix syntax``() =
        this.Parse """
module Program

type T = int option
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for F# ref type prefix syntax``() =
        this.Parse """
module Program

type T = ref<int>
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 9))

    [<Test>]
    member this.``No error for F# ref type postfix syntax``() =
        this.Parse """
module Program

type T = int ref
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for F# array type prefix syntax``() =
        this.Parse """
module Program

type T = array<int>
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 9))

    [<Test>]
    member this.``Error for F# array type standard postfix syntax``() =
        this.Parse """
module Program

type T = int array
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 9))

    [<Test>]
    member this.``No error for F# array type special postfix syntax``() =
        this.Parse """
module Program

type T = int []
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for generic type postfix syntax``() =
        this.Parse """
module Program

type X = int Generic
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 9))

    [<Test>]
    member this.``No error for generic type prefix syntax``() =
        this.Parse """
module Program

type X = Generic<int>
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Quickfix for F# List type``() = 
        let source = """
module Program

type T = list<int>
"""

        let expected = """
module Program

type T = int list
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Quickfix for F# Option type``() = 
        let source = """
module Program

type T = option<int>
"""

        let expected = """
module Program

type T = int option
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Quickfix for F# Ref type``() = 
        let source = """
module Program

type T = ref<int>
"""

        let expected = """
module Program

type T = int ref
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Quickfix for F# array type from prefix syntax``() = 
        let source = """
module Program

type T = array<int>
"""

        let expected = """
module Program

type T = int []
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Quickfix for F# array type from standard postfix syntax``() = 
        let source = """
module Program

type T = int array
"""

        let expected = """
module Program

type T = int []
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)
        
    [<Test>]
    member this.``Quickfix for F# array tuple type from standard postfix syntax``() = 
        let source = """
module Program

type T = (int * int) array
"""

        let expected = """
module Program

type T = (int * int) []
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)       

    [<Test>]
    member this.``Quickfix for generic type``() = 
        let source = """
module Program

type T = int Generic
"""

        let expected = """
module Program

type T = Generic<int>
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)
                 
    [<Test>]
    member this.``Error for missing space between module declarations``() =
        this.Parse """
module Program

let x = 1

let y = 2
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 0))

    [<Test>]
    member this.``Error for no space between module declarations``() =
        this.Parse """
module Program

let x = 1
let y = 2
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 0))

    [<Test>]
    member this.``No error for correct spacing between module declarations``() =
        this.Parse """
module Program

let x = 1


let y = 2
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``No error for correct spacing between module declarations with comments``() =
        this.Parse """
module Program

/// Comment 1
let x = 1


/// Comment 2
let y = 2
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for no space between class members``() =
        this.Parse """
module Program

type T = T of int with 
    static member x = 1
    static member x = 2
"""

        Assert.IsTrue(this.ErrorExistsAt(6, 0))

    [<Test>]
    member this.``No error for correct spacing between class members``() =
        this.Parse """
module Program

type T = T of int with
    static member x = 1

    static member x = 2
"""

        Assert.IsTrue(this.NoErrorsExist)
        
    [<Test>]
    member this.``No error for correct spacing between class members with comment``() =
        this.Parse """
module Program

type GenericOptions =
    { Props : IHTMLProp list
      Classes : string list }

    member this.AddModifiers(modifiers) =
        ()

    /// Conver to standard element
    member this.ToReactElement(el:IHTMLProp list -> ReactElement list -> ReactElement, ?children): ReactElement =
        ()

    /// Convert to self closing element
    member this.ToReactElement(el:IHTMLProp list -> ReactElement): ReactElement =
        ()
"""

        Assert.IsTrue(this.NoErrorsExist)       

    [<Test>]
    member this.``Error for too much spacing between class members``() =
        this.Parse """
module Program

type T = T of int with
    static member x = 1



    static member x = 2
"""

        Assert.IsTrue(this.ErrorExistsAt(6, 0))

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