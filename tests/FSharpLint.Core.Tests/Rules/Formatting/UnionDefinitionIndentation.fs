module FSharpLint.Core.Tests.Rules.Formatting.UnionDefinitionIndentation

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFormattingUnionDefinitionIndentation() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UnionDefinitionIndentation.rule)

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
    member this.``No error for correctly indented union definition cases``() =
        this.Parse"""
module Program

type T =
    | T1 of int
    | T2 of int
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``No error for correctly indented union definition cases in multi-type declaration``() =
        this.Parse"""
module Program

type T =
    | T1 of int
    | T2 of int
and V =
    | V1 of int
    | V2 of int
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``No error for correctly indented union definition cases with attribute``() =
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
