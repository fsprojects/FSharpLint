module FSharpLint.Core.Tests.Rules.Conventions.EnsureTailCallDiagnosticsInRecursiveFunctions

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

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
        ()
"""

        Assert.IsTrue <| this.ErrorExistsAt(2, 8)

    [<Test>]
    member this.``When nested function is recursive, should error telling to put [<TailCall>] attribute and make function non-nested``() =
        this.Parse """
let Bar () =
    let rec Foo someParam =
        if someParam then
            Foo false
        else
            ()
    ()
"""
        Assert.IsTrue <| this.ErrorExistsAt(3, 12)
        StringAssert.Contains("nested", this.ErrorMsg)

    [<Test>]
    member this.``Should not error when function is recursive and has [<TailCall>] attribute``() =
        this.Parse """
[<TailCall>]
let rec Foo someParam =
    if someParam then
        Foo false
    else
        ()
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should not error when function is defined with rec keyword but is not recursive``() =
        this.Parse """
let rec Foo someParam =
    ()

module Bar =
    let Foo = 0
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should error when functions are mutually recursive, but one of them has no [<TailCall>] attribute``() =
        this.Parse """
[<TailCall>]
let rec Foo someParam =
    if someParam then
        Foo false
    else
        Bar()
and Bar () =
    Foo true
"""
        
        Assert.IsTrue <| this.ErrorExistsAt(8, 4)

    [<Test>]
    member this.``Should not error when functions are mutually recursive, and both of them have [<TailCall>] attribute``() =
        this.Parse """
[<TailCall>]
let rec Foo someParam =
    if someParam then
        Foo false
    else
        Bar()
and [<TailCall>] Bar () =
    Foo true
"""
        
        Assert.IsTrue this.NoErrorsExist
