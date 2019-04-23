module FSharpLint.Core.Tests.Rules.Binding.UselessBinding

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestBindingUselessBinding() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UselessBinding.rule)

    [<Test>]
    member this.UselessBinding() = 
        this.Parse("""
module Program

let a = 10
let a = a""")

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.NotUselessBindingAsShadowingMutableWithImmutable() = 
        this.Parse """
module Program

let mutable a = 10
let a = a"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.NotUselessBindingAsShadowingImmutableWithMutable() = 
        this.Parse """
module Program

let a = 10
let mutable a = a"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.UselessBindingSuppressed() = 
        this.Parse """
module Program

let a = 10
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Binding", "UselessBinding")>]
let a = a"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.UselessBindingWithParens() = 
        this.Parse("""
module Program

let a = 10
let ((a)) = ((a))""")

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/101
    /// (a use binding will dispose the value so is not useless)
    [<Test>]
    member this.UseBindingWithSameNameDoesNotCauseUselessBindingError() = 
        this.Parse("""
module Program

type Cat() =
    static member CreateList(reader:TextReader) = 
        use reader = reader
        reader.ReadToEnd()""")
        
        Assert.IsFalse(this.ErrorsExist)
 