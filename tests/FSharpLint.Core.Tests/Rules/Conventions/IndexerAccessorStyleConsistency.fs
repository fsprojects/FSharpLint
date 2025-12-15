module FSharpLint.Core.Tests.Rules.Conventions.IndexerAccessorStyleConsistency

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests
open FSharpLint.Rules.IndexerAccessorStyleConsistency

[<TestFixture>]
type TestConventionsIndexerAccessorStyleConsistencyCSharp() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(IndexerAccessorStyleConsistency.rule { Style = IndexerAccessorStyle.CSharp })
    
    [<Test>]
    member this.IndexerAccessorStyleConsistencyOCamlStyleWhenUsingCSharp() =
        this.Parse """
module Program

let someArray = [| "foo" ; "bar" |]
let bar = someArray.[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.ErrorsExist
    
    [<Test>]
    member this.IndexerAccessorStyleConsistencyCSharpStyleWhenUsingCSharp() =
        this.Parse """
module Program

let someArray = [| "foo" ; "bar" |]
let bar = someArray[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.NoErrorsExist

[<TestFixture>]
type TestConventionsIndexerAccessorStyleConsistencyOCaml() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(IndexerAccessorStyleConsistency.rule { Style = IndexerAccessorStyle.OCaml })

    [<Test>]
    member this.IndexerAccessorStyleConsistencyCSharpStyleWhenUsingOCaml() =
        this.Parse """
module Program

let someArray = [| "foo" ; "bar" |]
let bar = someArray[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.IndexerAccessorStyleConsistencyCSharpStyleWhenUsingOCaml2() =
        this.Parse """
module Program

let withoutPrefix = [ "foo" ]
let firstChar = withoutPrefix[0] |> string
let rest = withoutPrefix.Substring 1
prefix + firstChar + rest"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.IndexerAccessorStyleConsistencyOCamlStyleWhenUsingOCaml() =
        this.Parse """
module Program

let someArray = [| "foo" ; "bar" |]
let bar = someArray.[1]
System.Console.WriteLine bar"""

        Assert.IsTrue this.NoErrorsExist
