module TestFormattingRules

open NUnit.Framework
open FSharpLint.Rules.Formatting
open FSharpLint.Framework.Configuration

let config = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.ofList 
                [ ("TypedItemSpacing", ruleEnabled)
                  ("TupleCommaSpacing", ruleEnabled)
                  ("TupleParentheses", ruleEnabled)
                  ("TypePrefixing", ruleEnabled)
                  ("ModuleDeclSpacing", ruleEnabled)
                  ("ClassMemberSpacing", ruleEnabled)
                  ("PatternMatchClausesOnNewLine", ruleEnabled)
                  ("PatternMatchOrClausesOnNewLine", ruleEnabled)
                  ("PatternMatchClauseIndentation", ruleEnabled)
                  ("PatternMatchExpressionIndentation", ruleEnabled) ]
              Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]
 
              
[<TestFixture>]
type TestFormatting() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

    [<Test>]
    member this.``Error for typed pattern without spaces around colon``() = 
        this.Parse("""
module Program

let (x:int) = 1""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.``No error for typed pattern with spaces around colon``() = 
        this.Parse("""
module Program

let (x : int) = 1""")

        Assert.IsTrue(this.NoErrorsExist)

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

    
    [<Test>]
    member this.``Error for tuple instantiation without parentheses``() =
        this.Parse("""
module Program

let x = 1, 2""")

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.``Quickfix for tuple instantiation without parentheses``() =
        let source = """
module Program

let x = 1, 2"""

        let expected = """
module Program

let x = (1, 2)"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``No error for tuple instantiation with parentheses``() =
        this.Parse("""
module Program

let x = (1, 2)""")

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for tuple instantiation without space after comma``() =
        this.Parse("""
module Program

let x = (1,2)""")

        Assert.IsTrue(this.ErrorExistsAt(4, 9))

    [<Test>]
    member this.``Quickfix for tuple instantiation without space after comma``() =
        let source = """
module Program

let x = (1,2)"""


        let expected = """
module Program

let x = (1, 2)"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Error for tuple instantiation with two spaces after comma``() =
        this.Parse("""
module Program

let x = (1,  2)""")

        Assert.IsTrue(this.ErrorExistsAt(4, 9))

    [<Test>]
    member this.``Quickfix for tuple instantiation with two spaces after comma``() =
        let source = """
module Program

let x = (1,  2)"""


        let expected = """
module Program

let x = (1, 2)"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``No error for tuple instantiation with single space after comma``() =
        this.Parse("""
module Program

let x = (1, 2)""")

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for pattern match clauses on same line``() =
        this.Parse("""
module Program

match 1 with
| 1 -> 1 | 2 -> 2""")

        Assert.IsTrue(this.ErrorExistsAt(5, 11))

    [<Test>]
    member this.``No error for pattern match clauses on different lines``() =
        this.Parse("""
module Program

match 1 with
| 1 -> 1
| 2 -> 2""")

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for pattern match or clauses on same line``() =
        this.Parse("""
module Program

match 1 with
| 1 | 2 -> 2""")

        Assert.IsTrue(this.ErrorExistsAt(5, 6))

    [<Test>]
    member this.``No error for pattern match or clauses on different lines``() =
        this.Parse("""
module Program

match 1 with
| 1
| 2 -> 2""")

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for exception match clauses on same line``() =
        this.Parse("""
module Program

try
    2/0
with
| :? System.DivideByZeroException -> 1 | :? System.Exception -> 2 """)

        Assert.IsTrue(this.ErrorExistsAt(7, 41))

    [<Test>]
    member this.``No error for exception match clauses on same line``() =
        this.Parse("""
module Program

try
    2/0
with
| :? System.DivideByZeroException -> 1
| :? System.Exception -> 2 """)

        Assert.IsTrue(this.NoErrorsExist)

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
    member this.``Error for pattern match clauses at different indentation``() =
        this.Parse"""
module Program

match 1 with
| 1 -> true
    | 2 -> false
"""

        Assert.IsTrue(this.ErrorExistsAt(6, 6))

    [<Test>]
    member this.``No error for pattern match clauses with same indentation``() =
        this.Parse"""
module Program

match 1 with
| 1 -> true
| 2 -> false
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for exception pattern match clauses at different indentation``() =
        this.Parse """
module Program

try
    2/0
with
    | :? System.DivideByZeroException -> 1
    | :? System.Exception -> 2 """

        Assert.IsTrue(this.ErrorExistsAt(7, 6))

    [<Test>]
    member this.``No error for exception pattern match clauses with same indentation``() =
        this.Parse """
module Program

try
    2/0
with
| :? System.DivideByZeroException -> 1
| :? System.Exception -> 2 """

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for lambda pattern match clauses at different indentation``() =
        this.Parse"""
module Program

1 |> (function
    | 1 -> true
        | 2 -> false)
"""

        Assert.IsTrue(this.ErrorExistsAt(6, 10))

    [<Test>]
    member this.``No error for lambda pattern match clauses with same indentation``() =
        this.Parse"""
module Program

1 |> (function
    | 1 -> true
    | 2 -> false)
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for lambda pattern match clauses without level of indentation for clauses``() =
        this.Parse"""
module Program

1 |> (function
| 1 -> true
| 2 -> false)
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.``Error for pattern match clauses indentation for expression on newline``() =
        this.Parse"""
module Program

match 1 with
| 1 -> 
true
| 2 -> 
    false)
"""

        Assert.IsTrue(this.ErrorExistsAt(6, 0))

    [<Test>]
    member this.``No error for pattern match clauses with indentation for expression on newline``() =
        this.Parse"""
module Program

match 1 with
| 1 -> 
    true
| 2 -> 
    false)
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for exception pattern match clauses indentation for expression on newline``() =
        this.Parse """
module Program

try
    2/0
with
| :? System.DivideByZeroException ->
1
| :? System.Exception ->
    2 """

        Assert.IsTrue(this.ErrorExistsAt(8, 0))

    [<Test>]
    member this.``No error for exception pattern match clauses with indentation for expression on newline``() =
        this.Parse """
module Program

try
    2/0
with
| :? System.DivideByZeroException ->
    1
| :? System.Exception ->
    2 """

        Assert.IsTrue(this.NoErrorsExist)

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
    member this.``Error for too much spacing between class members``() =
        this.Parse """
module Program

type T = T of int with
    static member x = 1



    static member x = 2
"""

        Assert.IsTrue(this.ErrorExistsAt(6, 0))