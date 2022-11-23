module FSharpLint.Core.Tests.Rules.Conventions.UsedUnderscorePrefixedElements

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsUsedUnderscorePrefixedElementsZahra() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UsedUnderscorePrefixedElements.rule)

    [<Test>]
    member this.``Lint flags record member(s) with type hints``() =
        this.Parse """
module MyModule =
    let MyFunc () =
        let _random = System.Random()
        printfn "%A" _random
        () """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``New keyword not considered unnecassery if used with a constructor of a type which implements IDisposable.``() =
        this.Parse("""
module Program

let _ = new System.IO.MemoryStream()""")

        this.AssertNoWarnings()

