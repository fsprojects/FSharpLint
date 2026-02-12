module FSharpLint.Core.Tests.Rules.Conventions.FavourLocalOverPrivate

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFavourLocalOverPrivate() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourLocalOverPrivate.rule)

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
    member this.``Top level private values that are not used in another function should not give an error`` () =
        this.Parse """
let private foo = 0

let Bar () =
    ()
"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Top level private value that is used in single function should give an error`` () =
        this.Parse """
let private foo = ()

let Bar () =
    foo
    ()
"""
        
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Private value that is used in more than one function should not give an error`` () =
        this.Parse """
let private foo = ()

let Bar () =
    foo
    ()

let Baz () =
    foo
    ()
"""
        
        this.AssertNoWarnings()

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
    
    [<Test>]
    member this.``Private literal should not give an error because literals can only be module-level`` () =
        this.Parse """
let [<Literal>] private Foo = 6

let Bar () =
    Foo
"""
        
        this.AssertNoWarnings()

    // Using attributes on nested functions (e.g [<TailCall>]) will give syntax error:
    // Unexpected symbol '[<' in expression
    [<Test>]
    member this.``Top level private function with attributes should not give an error`` () =
        this.Parse """
[<TailCall>]
let rec private Foo x =
    if x = 0 then
        Foo (x - 1)
    else
        0

let Bar () =
    Foo 3 |> ignore
"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Top level private function used in 2 or more functions with attributes should not give an error`` () =
        this.Parse """
let private foo x =
    x

[<TailCall>]
let rec private bar y =
    if foo y > 0 then
        bar (y - 1)
    else
        0

let baz () =
    foo 0
"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Top level private function used in 2 or more functions/methods should not give an error`` () =
        this.Parse """
let private foo x =
    x

type Bar() =
    member self.Foobar() =
        foo 1

let baz () =
    foo 0
"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Top level private function used in 1 methods should give an error`` () =
        this.Parse """
let private foo x =
    x

type Bar() =
    member self.Foobar() =
        foo 1
"""
        
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Top level private function used in 1 method 2 times should give an error`` () =
        this.Parse """
let private foo x =
    x

type Bar() =
    member self.Foobar() =
        let baz () =
            foo 2
        foo 1
"""
        
        Assert.IsTrue this.ErrorsExist


    [<Test>]
    member this.``Top level private function used in 2 or more functions (including nested modules) should not give an error`` () =
        this.Parse """
let private foo x =
    x

module Bar =
    let foobar () =
        foo 1

let baz () =
    foo 0
"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Top level private function used in 1 function in a nested module should give an error`` () =
        this.Parse """
let private foo x =
    x

module Bar =
    let foobar () =
        foo 1
"""
        
        Assert.IsTrue this.ErrorsExist
