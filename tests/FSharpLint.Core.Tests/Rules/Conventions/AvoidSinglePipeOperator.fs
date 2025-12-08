module FSharpLint.Core.Tests.Rules.Conventions.AvoidSinglePipeOperator

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsAvoidSinglePipeOperator() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidSinglePipeOperator.rule)

    [<Test>]
    member this.``Use pipe operator once``() =
        this.Parse """
let someFunc someParam =
    someParam
    |> someOtherFunc
"""

        Assert.IsTrue <| this.ViolationExistsAt(4, 4)

    [<Test>]
    member this.``Use pipe operator twice``() =
        this.Parse """
let someFunc someParam =
    someParam
    |> someOtherFunc
    |> yetAnotherFunc
"""

        Assert.IsFalse this.ViolationsExist

    [<Test>]
    member this.``Use pipe operator once in module``() =
        this.Parse """
module MyModule =
    let someFunc someParam =
        someParam
        |> someOtherFunc
"""

        Assert.IsTrue <| this.ViolationExistsAt(5, 8)

    [<Test>]
    member this.``Use pipe operator twice in module``() =
        this.Parse """
module MyModule =
    let someFunc someParam =
        someParam
        |> someOtherFunc
        |> yetAnotherFunc
"""

        Assert.IsFalse this.ViolationsExist

    [<Test>]
    member this.``Use pipe operator once in type``() =
        this.Parse """
type CustomerName(firstName) =
    member this.someFunc someParam =
        someParam
        |> someOtherFunc
"""

        Assert.IsTrue <| this.ViolationExistsAt(5, 8)

    [<Test>]
    member this.``Use pipe operator twice in type``() =
        this.Parse """
type CustomerName(firstName) =
    member this.someFunc someParam =
        someParam
        |> someOtherFunc
        |> yetAnotherFunc
"""

        Assert.IsFalse this.ViolationsExist

    [<Test>]
    member this.``Use pipe operator once to avoid parenthesis``() =
        this.Parse """
let someFunc someParam =
    someOtherFunc1 someParam
    |> someOtherFunc2
"""

        Assert.IsFalse this.ViolationsExist


    [<Test>]
    member this.``Use pipe operator once to avoid parenthesis on expression with function application``() =
        this.Parse """
let someFunc someParam =
    someOtherFunc1 someParam someParam2 |> someOtherFunc3
"""

        Assert.IsFalse this.ViolationsExist

    [<Test>]
    member this.``Use pipe operator after an if expression``() =
        this.Parse """
let foo param =
    if param then true else false
    |> bar
"""

        Assert.True this.NoViolationsExist

    [<Test>]
    member this.``Use pipe operator once on record within a if statement``() =
        this.Parse """
type Person =
    {
        FirstName: string
    }

let someFunc someParam =
    if someParam then
        { FirstName = "Bar" } |> someOtherFunc
    else
        Array.empty
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``Use pipe operator twice on record within a if statement``() =
        this.Parse """
type Person =
    {
        FirstName: string
    }

let someFunc someParam =
    if someParam then
        { FirstName = "Bar" }
        |> someOtherFunc
        |> yetAnotherFunc
    else
        Array.empty
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.``Use pipe operator once inside of an array``() =
        this.Parse """
let someFunc () =
    [| "Foo" |> String.length |]
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``Use pipe operator once on record within a nested if statement``() =
        this.Parse """
type Person =
    {
        FirstName: string
    }

let someFunc someParam barParam =
    if someParam then
        if barParam then
            { FirstName = "Bar" } |> someOtherFunc
        else
            Array.empty
    else
        Array.empty
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``Use pipe operator once without binding``() =
        this.Parse """
module Foo

-1.0 |> abs
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``Use pipe operator twice without binding``() =
        this.Parse """
module Foo

-1.0 |> abs |> ignore
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.``Use pipe operator thrice without binding``() =
        this.Parse """
module Foo

-1.0 |> printf "%d" |> ignore |> someOtherFunc
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.``Suggest not using pipe operator if it's used once``() =
        let source = """
let someFunc someParam =
    someParam
    |> someOtherFunc
"""
        let expected = """
let someFunc someParam =
    someOtherFunc someParam
"""
        
        this.Parse source
        let fixedSource = this.ApplyQuickFix source

        Assert.AreEqual(expected, fixedSource)

    [<Test>]
    member this.``Use pipe operator for function with more than 1 argument``() =
        this.Parse """
let someFunc someParam =
    someParam
    |> someOtherFunc someOtherParam
"""
        
        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.``Use pipe operator for higher-order function with more than 1 argument``() =
        this.Parse """
parsedSolution.ProjectsInOrder 
|> Seq.map(fun proj ->
    proj.AbsolutePath |> normalizeDirSeparatorsPaths |> FileInfo
)
"""
        
        Assert.IsTrue this.NoViolationsExist
