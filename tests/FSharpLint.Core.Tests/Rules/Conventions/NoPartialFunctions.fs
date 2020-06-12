module FSharpLint.Core.Tests.Rules.Conventions.NoPartialFunctions

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsNoPartialFunctions() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(NoPartialFunctions.rule)

    [<Test>]
    member this.``Error for partial function which should be replaced with pattern matching``() =
        this.Parse("""
let x = Option.get None""")

        this.AssertErrorWithMessageExists("Consider using pattern matching instead of partial function 'Option.get'")

    [<Test>]
    member this.``Error for partial function which should be replaced with another function``() =
        this.Parse("""
let x = List.find 1 [2; 3; 4]""")

        this.AssertErrorWithMessageExists( "Consider using 'List.tryFind' instead of partial function 'List.find'")
