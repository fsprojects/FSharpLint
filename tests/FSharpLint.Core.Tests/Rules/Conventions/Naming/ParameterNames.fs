module FSharpLint.Core.Tests.Rules.Conventions.ParameterNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Core.Tests

let config =
    { NamingConfig.Naming = Some NamingCase.CamelCase
      Underscores = Some NamingUnderscores.AllowPrefix
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsParameterNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ParameterNames.rule config)

    [<Test>]
    member this.FunctionParameterIsCamelCase() =
        this.Parse """
module Program

let main dog = ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.ConstructorParameterIsPascalCase() =
        this.Parse """
module Program

type MyClass2(Cats) as this =
    member this.PrintMessage() = ()
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 14))

    [<Test>]
    member this.ConstructorParameterIsCamelCase() =
        this.Parse """
module Program

type MyClass2(cats) as this =
    member this.PrintMessage() = ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.CompilerGeneratedArgumentName() =
        this.Parse """
module Program

(fun _ -> ())
"""

        this.AssertNoViolations()

    [<Test>]
    member this.ParameterUnionCaseContainingValueDoesNotGenerateViolation() =
        this.Parse """
module Program

type SingleCaseDU = SingleCaseDU of int

let extractInt (SingleCaseDU myInt) =
    myInt

let singleCaseDU = SingleCaseDU 5

let result = extractInt singleCaseDU
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``Auto fix for underscores with config of `AllowPrefix` will only remove underscores not prefixing the identifier.``() =
        let source = """
module Program

let baz __foo_bar = 0
"""

        let expected = """
module Program

let baz __foobar = 0
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyAutoFix source)

    [<Test>]
    member this.``Auto fix for camel case takes into account underscore prefixes.``() =
        let source = """
module Program

let foo _X = 0
"""

        let expected = """
module Program

let foo _x = 0
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyAutoFix source)

    [<Test>]
    member this.``As pattern is processed correctly (GetPatternIdents regression)``() =
        this.Parse """
let foo ((x, y) as bar_coord) = bar_coord
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``Module members should not cause violations as they are not parameters``() =
        this.Parse """
module BitLaunch =
    module Regions =
        let Bucharest = "Bucharest"
        let Amsterdam someArg = "Amsterdam"
"""
        
        Assert.IsFalse this.ViolationsExist
