module FSharpLint.Core.Tests.Rules.Formatting.PatternMatchExpressionIndentation

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestFormattingPatternMatchExpressionIndentation() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(PatternMatchExpressionIndentation.rule)

    [<Test>]
    member this.``Error for pattern match clauses indentation for expression on newline``() =
        this.Parse"""
module Program

match 1 with
| 1 -> 
true
| 2 -> 
    false
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
    false
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
