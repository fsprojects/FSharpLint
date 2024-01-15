module FSharpLint.Core.Tests.Rules.Conventions.UnnestedFunctionNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }

[<TestFixture>]
type TestConventionsUnnestedFunctionNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UnnestedFunctionNames.rule config)

    [<Test>]
    member this.FunctionNameIsPascalCase() =
        this.Parse """
module Program =
    let CylinderVolume radius length =
        let pi = 3.14159
        length * pi * radius * radius
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FunctionNameHasUnderscore() =
        this.Parse """
module Program =
    let Cylinder_Volume radius length =
        let pi = 3.14159
        length * pi * radius * radius
"""

        Assert.IsTrue(this.ErrorExistsAt(3, 8))

    [<Test>]
    member this.FunctionNameIsCamelCase() =
        this.Parse """
module Program =
    let cylinderVolume radius length =
        let pi = 3.14159
        length * pi * radius * radius
"""

        Assert.IsTrue(this.ErrorExistsAt(3, 8))

    [<Test>]
    member this.FunctionNameWithNoArgsIsCamelCase() =
        this.Parse """
module Program =
    let cylinderVolume() =
        let pi = 3.14159
        length * pi * radius * radius
"""

        Assert.IsTrue(this.ErrorExistsAt(3, 8))

    [<Test>]
    member this.FunctionNameWithNoArgsIsPascalCase() =
        this.Parse """
module Program =
    let CylinderVolume() =
        let pi = 3.14159
        length * pi * radius * radius
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ValueNameWithNoArgsAndNoParenthesisIsCamelCase() =
        this.Parse """
module Program
    let cylinderVolume =
        let radius = 1
        let pi = 3.14159
        length * pi * radius * radius
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PrivateFunctionNameIsCamelCase() =
        this.Parse """
module Program =
    let private cylinderVolume() =
        let pi = 3.14159
        length * pi * radius * radius
"""

        Assert.IsTrue(this.ErrorExistsAt(3, 16))

    [<Test>]
    member this.PublicFunctionNameIsCamelCase() =
        this.Parse """
module Program =
    let public cylinderVolume() =
        let pi = 3.14159
        length * pi * radius * radius
"""

        Assert.IsTrue(this.ErrorExistsAt(3, 15))


    [<Test>]
    member this.PrivateFunctionNameIsPascalCase() =
        this.Parse """
module Program =
    let private CylinderVolume() =
        let pi = 3.14159
        length * pi * radius * radius
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FunctionNameInTypeIsCamelCase() =
        this.Parse """
type MyClass(initX:int) =
    let x = initX
    member this.method() = printf "x=%i" x
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 16))

    [<Test>]
    member this.FunctionNameInRecordIsCamelCase() =
        this.Parse """
type Record =
    { Dog: int }
    member this.cylinderVolume length radius =
        let pi = 3.14159
        length * pi * radius * radius
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 16))

    [<Test>]
    member this.NestedFunctionNameIsCamelCase() =
        this.Parse """
module Program =
    let CylinderVolume radius length =
        let nestedFunction arg1 =
            arg1 + 1

        let pi = 3.14159
        length * pi * radius * radius
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.NestedFunctionNameInTypeIsCamelCase() =
        this.Parse """
type Record =
    { Dog: int }
    member this.CylinderVolume length radius =
        let nestedFunction arg1 =
            arg1 + 1
        let pi = 3.14159
        length * pi * radius * radius
"""

        this.AssertNoWarnings()

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
        length * pi * radius * radius
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.UnnestedFunctionNameIsCamelCase() =
        this.Parse """
module Program =
    let CylinderVolume() =
        let radius = 1
        let pi = 3.14159
        length * pi * radius * radius

    let cylinderVolume2() =
        let radius = 1
        let pi = 3.14159
        length * pi * radius * radius
"""

        Assert.IsTrue(this.ErrorExistsAt(8, 8))
