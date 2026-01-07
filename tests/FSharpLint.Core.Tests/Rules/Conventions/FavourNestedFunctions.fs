module FSharpLint.Core.Tests.Rules.Conventions.FavourNestedFunctions

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFavourNestedFunctions() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourNestedFunctions.rule)

    [<Test>]
    member this.``Top level functions that are not used in another function should not give an error`` () =
        this.Parse """
let Foo () =
    ()

let Bar () =
    ()
"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Top level private functions that are not used in another function should not give an error`` () =
        this.Parse """
let private Foo () =
    ()

let Bar () =
    ()
"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Top level private function that is used in single function should give an error`` () =
        this.Parse """
let private Foo () =
    ()

let Bar () =
    Foo()
    ()
"""
        
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Top level recursive private function that is used in single other function should give an error`` () =
        this.Parse """
let rec private Foo x =
    if x = 0 then
        Foo (x - 1)
    else
        0

let Bar () =
    Foo 3 |> ignore
"""
        
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Top level recursive private function that is used in multiple other functions should not give an error`` () =
        this.Parse """
let rec private Foo x =
    if x = 0 then
        Foo (x - 1)
    else
        0

let rec Bar x =
    Foo x |> Baz
and Baz x =
    Bar (x - 1)

let FooBar () =
    Foo 4
"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Nested functions should not give an error`` () =
        this.Parse """
let Bar () =
    let Foo() =
        ()

    Foo()
    ()
"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Private function that is used in more than one function should not give an error`` () =
        this.Parse """
let private Foo () =
    ()

let Bar () =
    Foo()
    ()

let Baz () =
    Foo ()
    ()
"""
        
        this.AssertNoWarnings()
