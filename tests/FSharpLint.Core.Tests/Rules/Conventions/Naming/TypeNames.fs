module FSharpLint.Core.Tests.Rules.Conventions.TypeNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.naming = Some NamingCase.PascalCase
      underscores = Some NamingUnderscores.None
      prefix = None
      suffix = None }
[<TestFixture>]
type TestConventionsTypeNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(TypeNames.rule config)

    [<Test>]
    member this.TypeAbbreviationIsPascalCase() =
        this.Parse """
module Program
  type Cat = int"""

        this.AssertNoWarnings()

    [<Test>]
    member this.TypeAbbreviationIsCamelCase() =
        this.Parse """
module Program
  type cat = int"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.ClassNameIsPascalCase() =
        this.Parse """
module Program
  type MyClass2() as this =
    member this.PrintMessage() = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ClassNameIsCamelCase() =
        this.Parse """
module Program
  type myClass2() as this =
    member this.PrintMessage() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.UnionNameIsPascalCase() =
        this.Parse """
module Program
  type Union =
    | Some"""

        this.AssertNoWarnings()

    [<Test>]
    member this.UnionNameIsCamelCase() =
        this.Parse """
module Program
  type union =
    | Some"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.RecordNameIsPascalCase() =
        this.Parse """
module Program
  type Record = { Dog: int }"""

        this.AssertNoWarnings()

    [<Test>]
    member this.RecordNameIsCamelCase() =
        this.Parse """
module Program
  type record = { Dog: int }"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.DelegateNameIsPascalCase() =
        this.Parse """
module Program
  type Delegate2 = delegate of int * int -> int"""

        this.AssertNoWarnings()

    [<Test>]
    member this.DelegateNameIsCamelCase() =
        this.Parse """
module program
  type delegate2 = delegate of int * int -> int"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.StructNameIsPascalCase() =
        this.Parse """
module Program
  type Point2D =
    struct
      val X: float
      val Y: float
      new(x: float, y: float) = { X = x; Y = y }
    end"""

        this.AssertNoWarnings()

    [<Test>]
    member this.StructNameIsCamelCase() =
        this.Parse """
module program
  type point2D =
    struct
      val X: float
      val Y: float
      new(x: float, y: float) = { X = x; Y = y }
    end"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.TypeExtensionTypeIsCamelCase() =
        this.Parse """
module Program

type myClass() =
    member this.F() = 100

type myClass with
    member this.Goat() = 200"""

        Assert.IsFalse(this.ErrorExistsAt(7, 5))

    [<Test>]
    member this.TypeExtensionTypeIsPascalCase() =
        this.Parse """
module Program

type MyClass() =
    member this.F() = 100

type MyClass with
    member this.Goat() = 200"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PascalCaseTypeAbbreviationOfLiteral() =
        this.Parse ("""
module Program

type Abbreviation = LiteralAttribute

[<Abbreviation>]
let Dog = 6""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Upper case international characters recognised by PascalCase rule``() =
        this.Parse """
module Program

type Ścieżka = Ścieżka of string
        """

        this.AssertNoWarnings()


    [<Test>]
    member this.``Quick fix for underscores with config of `None` when will remove prefixing underscores.``() =
        let source = """
module Program

type _Cat = | Foo
"""

        let expected = """
module Program

type Cat = | Foo
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Quick fix for pascal case converts the first character of the identifier to upper case.``() =
        let source = """
module Program

type cat = | Foo
"""

        let expected = """
module Program

type Cat = | Foo
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.EnumNameIsPascalCase() =
        this.Parse """
module Program
  type MyEnum =
    | EnumCase = 1"""

        this.AssertNoWarnings()

    [<Test>]
    member this.EnumNameIsCamelCase() =
        this.Parse """
module Program
  type myEnum =
    | EnumCase = 1"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

