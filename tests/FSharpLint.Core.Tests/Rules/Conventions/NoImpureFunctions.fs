module FSharpLint.Core.Tests.Rules.Conventions.NoImpureFunctions


open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsNoImpureFunctions() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(NoImpureFunctions.rule { AdditionalImpureFunctions = ["Custom.impure"]; AllowedImpureFunctions = ["Array.set"] })

    [<Test>]
    member this.``Error for impure function which should be replaced with another function``() =
        this.Parse("let x = Array.sortInPlace [2; 1; 4]")

        Assert.IsTrue this.ErrorsExist
    
    [<Test>]
    member this.``Error for user-specified impure function``() =
        this.Parse("let x = Custom.impure (ref 4)")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``No error for user-specified allowed partial function``() =
        this.Parse("let x = [| Some 4; None |]
Array.set x 0 None")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Error for creating a mutable list``() =
        this.Parse("let x = System.Collections.Generic.List<int>()")
        
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Error for creating a mutable list without explicit type param``() =
        this.Parse("let x = System.Collections.Generic.List [ 1; 2 ]")
        
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Error for creating a mutable list with new keyword``() =
        this.Parse("open System.Collections.Generic

let x = new List<int>()")
        
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``No error for using an existing mutable list``() =
        this.Parse("open System.Collections.Generic

type IHasList =
    abstract List: List<int>

let x (hasList: IHasList) = hasList.List")
        
        this.AssertNoWarnings()

