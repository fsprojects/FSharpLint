module FSharpLint.Core.Tests.Rules.Conventions.NestedFunctionNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.CamelCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }

[<TestFixture>]
type TestConventionsNestedFunctionNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(NestedFunctionNames.rule config)

    [<Test>]
    member this.UnnestedFunctionNameInPascalCaseMustBeIgnored() =
        this.Parse """
module Program =
    let CylinderVolume radius length =
        let pi = 3.14159
        length * pi * radius * radius"""

        this.AssertNoWarnings()

    [<Test>]
    member this.NestedFunctionNameIsCamelCase() =
        this.Parse """
module Program =
    let CylinderVolume radius length =
        let nestedFunction arg1 =
            arg1 + 1

        let pi = 3.14159
        length * pi * radius * radius"""

        this.AssertNoWarnings()

    [<Test>]
    member this.NestedFunctionNameIsPascalCase() =
        this.Parse """
module Program =
let CylinderVolume radius length =
    let NestedFunction arg1 =
        arg1 + 1

    let pi = 3.14159
    length * pi * radius * radius"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.NestedFunctionNameInTypeIsPascalCase() =
        this.Parse """
type Record =
    { Dog: int }
    member this.CylinderVolume length radius =
        let NestedFunction arg1 =
            arg1 + 1
        let pi = 3.14159
        length * pi * radius * radius"""

        Assert.IsTrue(this.ErrorExistsAt(5, 12))

    [<Test>]
    member this.UnnestedFunctionNameIsPascalCase() =
        this.Parse """
module Program =
    let CylinderVolume() =
        let radius = 1
        let pi = 3.14159
        length * pi * radius * radius

    let CylinderVolume2() =
        let radius = 1
        let pi = 3.14159
        length * pi * radius * radius"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PrivateUnnestedFunctionNameIsPascalCase() =
        this.Parse """
module Program =
    let private CylinderVolume() =
        let radius = 1
        let pi = 3.14159
        length * pi * radius * radius"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PrivateUnnestedFunctionNameIsCamelCase() =
        this.Parse """
module Program =
    let private cylinderVolume() =
        let radius = 1
        let pi = 3.14159
        length * pi * radius * radius"""

        this.AssertNoWarnings()
