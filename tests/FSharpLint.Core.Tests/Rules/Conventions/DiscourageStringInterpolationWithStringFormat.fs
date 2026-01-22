module FSharpLint.Core.Tests.Rules.Conventions.DiscourageStringInterpolationWithStringFormat

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsDiscourageStringInterpolationWithStringFormat() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(DiscourageStringInterpolationWithStringFormat.rule)

    [<Test>]
    member this.StringInterpolationWithSprintfShouldNotProduceError() =
        this.Parse """
let world = "world"
let someString = sprintf "Hello %s" world"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.StringInterpolationWithStringFormatShouldProduceError() =
        this.Parse """
let world = "world"
let someString = String.Format("Hello {0}", world)"""

        Assert.IsTrue this.ErrorsExist


    [<Test>]
    member this.StringInterpolationWithStringFormatAndExternalTemplateShouldNotProduceError() =
        this.Parse """
let someFunction someTemplate =
    Console.WriteLine(String.Format(someTemplate, world))"""

        Assert.IsTrue this.NoErrorsExist


    [<Test>]
    member this.StringInterpolationWithStringFormatAndLocalVariableShouldProduceError() =
        this.Parse """
let world = "world"
let someTemplate = "Hello {0}"
let someString = String.Format(someTemplate, world)"""

        Assert.IsTrue this.ErrorsExist


    [<Test>]
    member this.StringInterpolationWithMultipleModuleWithSameVariableNameShouldNotProduceError() =
        this.Parse """
module Foo =
    let someTemplate = "Hello {0}"
module Bar =
    let someFunction someTemplate =
        let world = "world"
        Console.WriteLine(String.Format(someTemplate, "world"))"""

        Assert.IsTrue this.NoErrorsExist


    [<Test>]
    member this.StringInterpolationWithSameVariableNameInInnerLetShouldNotProduceError() =
        this.Parse """
module Bar =
    let exampleFunction () =
        let someTemplate = "Hello {0}"
        someTemplate
    let someFunction someTemplate =
        let someNestedFunc () =
            1
        let world = "world"
        Console.WriteLine(String.Format(someTemplate, "world"))"""

        Assert.IsTrue this.NoErrorsExist


    [<Test>]
    member this.StringInterpolationWithSameVariableNameWithLocalLetShouldNotProduceError() =
        this.Parse """
module Bar =
    let exampleFunction someTemplate =
        let someResults =
            let someTemplate = "Hello {0}"
            someTemplate
        let world = "world"
        Console.WriteLine(String.Format(someTemplate, "world"))"""

        Assert.IsTrue this.NoErrorsExist
