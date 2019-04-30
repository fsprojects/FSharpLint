module FSharpLint.Core.Tests.Rules.Binding.WildcardNamedWithAsPattern

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestBindingWildcardNamedWithAsPattern() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(WildcardNamedWithAsPattern.rule)
    
    [<Test>]
    member this.WildcardNamedWithAsPattern() = 
        this.Parse """
module Program

match [] with
    | _ as x -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(5, 6))
        
    [<Test>]
    member this.WildcardNamedWithAsPatternSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Binding", "WildcardNamedWithAsPattern")>]
let f =
    match [] with
        | _ as x -> ()"""
        
        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.NamedPattern() = 
        this.Parse """
module Program

match [] with
    | x -> ()"""
    
        Assert.IsFalse(this.ErrorsExist)
   


