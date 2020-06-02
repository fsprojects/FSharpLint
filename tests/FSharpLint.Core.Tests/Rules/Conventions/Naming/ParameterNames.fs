module FSharpLint.Core.Tests.Rules.Conventions.ParameterNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.CamelCase
      Underscores = Some NamingUnderscores.AllowPrefix
      Prefix = None
      Suffix = None }
[<TestFixture>]
type internal TestConventionsParameterNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ParameterNames.rule config)

    [<Test>]
    member this.FunctionParameterIsCamelCase() =
        this.Parse """
module Program
  let main dog = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ConstructorParameterIsPascalCase() =
        this.Parse """
module Program
  type MyClass2(Cats) as this =
    member this.PrintMessage() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 16))

    [<Test>]
    member this.ConstructorParameterIsCamelCase() =
        this.Parse """
module Program
  type MyClass2(cats) as this =
    member this.PrintMessage() = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.CompilerGeneratedArgumentName() =
        this.Parse """
module Program
(fun _ -> ())
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ParameterUnionCaseContainingValueDoesNotGenerateWarning() =
        this.Parse("""
module Program

type SingleCaseDU = SingleCaseDU of int
let extractInt (SingleCaseDU myInt) =
  myInt

let singleCaseDU = SingleCaseDU 5

let result = extractInt singleCaseDU""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Quick fix for underscores with config of `AllowPrefix` will only remove underscores not prefixing the identifier.``() =
        let source = """
module Program

let __foo_bar = 0
"""

        let expected = """
module Program

let __foobar = 0
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Quick fix for camel case takes into account underscore prefixes.``() =
        let source = """
module Program

let foo _X = 0
"""

        let expected = """
module Program

let foo _x = 0
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)
