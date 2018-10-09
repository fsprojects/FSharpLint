module TestTypesRules

open NUnit.Framework
open FSharpLint.Rules.Types
open FSharpLint.Framework.Configuration

let config = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.ofList 
                [ ("TypePrefixing", ruleEnabled) ]
              Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]

[<TestFixture>]
type TestBindingRules() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

    [<Test>]
    member this.``Error for F# List type prefix syntax``() =
        this.Parse """
module Program

type T = list<int>
"""

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