module FSharpLint.Core.Tests.Rules.Conventions.EnsureTailCallDiagnosticsInRecursiveFunctions

open NUnit.Framework

open FSharpLint.Rules

[<TestFixture>]
type TestEnsureTailCallDiagnosticsInRecursiveFunctions() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(EnsureTailCallDiagnosticsInRecursiveFunctions.rule)

    [<Test>]
    member this.``Should error when function is recursive, but has no [<TailCall>] attribute``() =
        this.Parse """
let rec Foo someParam =
    if someParam then
        Foo false
    else
        ()"""

        Assert.IsTrue <| this.ErrorExistsAt(2, 8)

    [<Test>]
    member this.``Should not error when function is recursive and has [<TailCall>] attribute``() =
        this.Parse """
[<TailCall>]
let rec Foo someParam =
    if someParam then
        Foo false
    else
        ()"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should error when function is defined with rec keyword but is not recursive``() =
        this.Parse """
let rec Foo someParam =
    ()

module Bar =    
    let Foo = 0"""

        Assert.IsTrue 
            <| this.ErrorWithMessageExistsAt(
                """The 'Foo' function has a "rec" keyword, but it is not really recursive, consider removing it.""", 
                2, 
                8)
