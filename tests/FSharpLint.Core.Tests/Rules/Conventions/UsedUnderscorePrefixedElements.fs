module FSharpLint.Core.Tests.Rules.Conventions.UsedUnderscorePrefixedElements

open NUnit.Framework

open FSharpLint.Rules

[<TestFixture>]
type TestConventionsUsedUnderscorePrefixedElements() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UsedUnderscorePrefixedElements.rule)

    [<Test>]
    member this.``Use variable with underscore prefix``() =
        this.Parse """
module MyModule =
    let MyFunc () =
        let _random = System.Random()
        printfn "%A" _random
        ()
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Use variable with underscore prefix but not immediately after declaration``() =
        this.Parse """
module MyModule =
    let MyFunc () =
        let _random = System.Random()
        ()
        ()
        _random
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Use variable with underscore prefix inside some expression``() =
        this.Parse """
module MyModule =
    let MyFunc () =
        let _random = System.Random()
        "someString" + _random.ToString()
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Use variable with underscore prefix defined in tuple``() =
        this.Parse """
module MyModule =
    let MyFunc () =
        let (_random, x) = System.Random()
        printfn "%A" _random
        ()
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Not using variable with underscore prefix``() =
        this.Parse """
module MyModule =
    let MyFunc () =
        let _random = System.Random()
        printfn "_random is not used here"
        ()
"""

        Assert.IsFalse this.ErrorsExist
        
    [<Test>]
    member this.``Using variable without underscore prefix``() =
        this.Parse """
module MyModule =
    let MyFunc () =
        let random = System.Random()
        printfn "%A" random
        ()
"""

        Assert.IsFalse this.ErrorsExist

    [<Test>]
    member this.``Use of underscore variable in function should not cause error``() =
        this.Parse """
module MyModule =
    let MyFunc () =
        let f _ = System.Random()
        ()
"""

        Assert.IsFalse this.ErrorsExist
        
    [<Test>]
    member this.``Using field with underscore prefix``() =
        this.Parse """
type CustomerName(firstName) =
    member this._FirstName = firstName
    member this.MyFunc () =
        printfn "%A" this._FirstName
"""

        Assert.IsFalse this.ErrorsExist
        
    [<Test>]
    member this.``Using private member with underscore prefix``() =
        this.Parse """
type CustomerName(firstName) =
    member private this._FirstName = firstName
    member this.MyFunc () =
        printfn "%A" this._FirstName
"""

        Assert.IsFalse this.ErrorsExist

    [<Test>]
    member this.``Using private property with underscore prefix``() =
        this.Parse """
type CustomerName(firstName) =
    member val private _FirstName = firstName with get, set
    
    member this.MyFunc () =
        printfn "%A" this._FirstName
"""

        Assert.IsFalse this.ErrorsExist
