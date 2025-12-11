module FSharpLint.Core.Tests.Rules.Binding.UselessBinding

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestBindingUselessBinding() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UselessBinding.rule)

    [<Test>]
    member this.UselessBinding() =
        this.Parse """
module Program

let a = 10
let a = a
"""

        Assert.IsTrue(this.ViolationExistsAt(5, 4))

    [<Test>]
    member this.NotUselessBindingAsShadowingMutableWithImmutable() =
        this.Parse """
module Program

let mutable a = 10
let a = a
"""

        Assert.IsFalse(this.ViolationsExist)

    [<Test>]
    member this.NotUselessBindingAsShadowingImmutableWithMutable() =
        this.Parse """
module Program

let a = 10
let mutable a = a
"""

        Assert.IsFalse(this.ViolationsExist)

    [<Test>]
    member this.UselessBindingWithParens() =
        this.Parse """
module Program

let a = 10
let ((a)) = ((a))
"""

        Assert.IsTrue(this.ViolationExistsAt(5, 4))

    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/101
    /// (a use binding will dispose the value so is not useless)
    [<Test>]
    member this.UseBindingWithSameNameDoesNotCauseUselessBindingViolation() =
        this.Parse """
module Program

type Cat() =
    static member CreateList(reader:TextReader) =
        use reader = reader
        reader.ReadToEnd()
"""

        Assert.IsFalse(this.ViolationsExist)

    [<Test>]
    member this.UselessBindingAutoFix() =
        let source = """
module Program

let a = 10
let a = a"""
        let expected = """
module Program

let a = 10
"""

        this.Parse source

        Assert.IsTrue(this.ViolationExistsAt(5, 4))

        let result = this.ApplyAutoFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.UselessBindingWithParensAutoFix() =
        let source = """
module Program

let a = 10
let ((a)) = ((a))"""

        let expected = """
module Program

let a = 10
"""

        this.Parse source

        Assert.IsTrue(this.ViolationExistsAt(5, 4))

        let result = this.ApplyAutoFix source

        Assert.AreEqual(expected, result)
