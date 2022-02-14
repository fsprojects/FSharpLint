module FSharpLint.Core.Tests.Rules.Conventions.AvoidTypeHintSuffixesInNames

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsAvoidTypeHintSuffixesInNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidTypeHintSuffixesInNames.rule)

    [<Test>]
    member this.``Lint flags record member(s) with type hints``() =
        this.Parse """
module Person =
    type FullName = { FirstName: string; SurnamesList: List<string> } """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Lint flags discriminated union member(s) with type hints``() =
        this.Parse """
type Tree =
    | Scalar
    | NodeList of int * Tree * Tree """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Lint flags autoproperty with type hints``() =
        this.Parse """
type MyClass() =
    let random  = new System.Random()
    member val RandomOpt = (random.Next() |> Some) with get, set """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Lint flags property with type hints``() =
        this.Parse """
type MyClass() =
   let mutable someString = "someString"
   member this.SomeStr with get() = someString and set(v : string) = someString <- v """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Lint does not flag discriminated union member(s) without type hints``() =
        this.Parse """
type Tree =
    | Tip
    | Node of int * Tree * Tree """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Lint only flags type hints when they are suffixes``() =
        this.Parse """
type Tree =
    | Opt
    | Node of int * Tree * Tree """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Lint does not flag record member(s) without type hints``() =
        this.Parse """
module Person =
    type FullName = { First: string; Last: string } """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Lint does not flag variable(s) even if they have type hints``() =
        this.Parse """
let getNothing () =
    let valOpt = 90
    valOpt """

        Assert.IsTrue this.NoErrorsExist
