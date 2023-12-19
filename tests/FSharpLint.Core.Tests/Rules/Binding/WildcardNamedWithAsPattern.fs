module FSharpLint.Core.Tests.Rules.Binding.WildcardNamedWithAsPattern

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestBindingWildcardNamedWithAsPattern() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(WildcardNamedWithAsPattern.rule)

    [<Test>]
    member this.WildcardNamedWithAsPattern() =
        let source = """
module Program

match [] with
| _ as x -> ()
"""

        let expected = """
module Program

match [] with
| x -> ()"""
        
        this.Parse source

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.NamedPattern() =
        this.Parse """
module Program

match [] with
| x -> ()
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.WildcardWithoutAsPattern() =
        this.Parse """
module Program

match [] with
| _ -> ()
"""

        Assert.IsFalse(this.ErrorsExist)



