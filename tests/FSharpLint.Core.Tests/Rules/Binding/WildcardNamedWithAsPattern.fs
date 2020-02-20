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
    member this.NamedPattern() =
        this.Parse """
module Program

match [] with
    | x -> ()"""

        Assert.IsFalse(this.ErrorsExist)



