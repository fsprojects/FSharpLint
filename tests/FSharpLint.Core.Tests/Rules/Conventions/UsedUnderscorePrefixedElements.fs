module FSharpLint.Core.Tests.Rules.Conventions.UsedUnderscorePrefixedElements

open NUnit.Framework
open FSharpLint.Rules
open System

[<TestFixture>]
type TestConventionsUsedUnderscorePrefixedElementsZahra() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UsedUnderscorePrefixedElements.rule)

    [<Test>]
    member this.``Use variable with underscore prefix``() =
        this.Parse """
module MyModule =
    let MyFunc () =
        let _random = System.Random()
        printfn "%A" _random
        () """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Not using variable with underscore prefix``() =
        this.Parse """
module MyModule =
    let MyFunc () =
        let _random = System.Random()
        printfn "_random is not used here"
        () """

        Assert.IsFalse this.ErrorsExist


