module FSharpLint.Core.Tests.Rules.Conventions.PrivateValuesNames

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
type TestConventionsPrivateValuesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(PrivateValuesNames.rule config)

    /// A tuple inside a binding should be treated as private.
    [<Test>]
    member this.TupleInsideBindingExprIsPascalCase() =
        this.Parse """
module Program

let main =
    let (Cat, _) = 1, 0
    ()
"""

        Assert.IsTrue(this.ViolationExistsAt(5, 9))

    [<Test>]
    member this.PrivateTupleIsPascalCase() =
        this.Parse """
module Program

let private Cat, private dog = 1, 0
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 12))

    [<Test>]
    member this.PrivateFunctionNameIsPascalCase() =
        this.Parse """
module Program

let private Main () = ()
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 12))

    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/103
    [<Test>]
    member this.MnemonicWildcardInPatternMatch() =
        this.Parse """
module Program

let main =
    match true with
    | _dog -> ()
    | _ -> ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.UnderscoreInMatchPatternIdent() =
        this.Parse """
module Program

let main =
    match true with
    | d_og -> ()
    | _ -> ()
"""

        Assert.IsTrue(this.ViolationExistsOnLine 6)

    [<Test>]
    member this.VariablePatternMatchIsCamelCase() =
        this.Parse """
module Program

let main =
    match true with
    | dog -> ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.PatternMatchAsIsPascalCase() =
        this.Parse """
module Program

let main =
    match true with
    | _ as Dog -> ()
"""

        Assert.IsTrue(this.ViolationExistsAt(6, 11))

    [<Test>]
    member this.PatternMatchAsIsCamelCase() =
        this.Parse """
module Program

let main =
    match true with
    | _ as dog -> ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.FunctionNameNestedInBindingIsPascalCase() =
        this.Parse """
module program

let main () =
    let Main () = ()
    ()
"""

        Assert.IsTrue(this.ViolationExistsAt(5, 8))

    [<Test>]
    member this.FunctionNameNestedInBindingIsCamelCase() =
        this.Parse """
module Program

let main () =
    let bain () = ()
    ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.CamelCaseLetBindingInType() =
        this.Parse """
module Program

type Dog() =
    let cat() = ()

    member this.Goat() = ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.PascalCaseLetBindingInType() =
        this.Parse """
module program

type Dog() =
    let Cat() = ()

    member this.Goat() = ()
"""

        Assert.IsTrue(this.ViolationExistsAt(5, 8))

    [<Test>]
    member this.PascalCaseLetBindingInMethod() =
        this.Parse """
module Program

type Cat() =
    member this.ContainsBinding() =
        let Goat = 0
        ()
"""

        Assert.IsTrue(this.ViolationExistsAt(6, 12))

    [<Test>]
    member this.CamelCaseLetBindingInMethod() =
        this.Parse """
module Program

type Cat() =
    member this.ContainsBinding() =
        let goat = 0
        ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.LiteralPatternMatchExpectNoViolations() =
        this.Parse """
module Program

[<Literal>]
let Dog = true

let main =
    match true with
    | Dog -> ()
    | _ -> ()
"""

        this.AssertNoViolations()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/191
    [<Test>]
    member this.``Backticked let binding identifier not checked by name convention rules``() =
        this.Parse """
module Program

let foo () =
    let ``¯\_(ツ)_/¯`` = ignore
    ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``Lower case international characters recognised by camelCase rule``() =
        this.Parse """
module Program

let foo () =
    let żcieżka = 0
    ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.FunctionParameterIsPascalCase() =
        this.Parse """
module Program

let main Dog = ()
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 9))

    [<Test>]
    member this.``Quick fix for camel case converts the first character of the identifier to lower case.``() =
        let source = """
module Program

let foo X = 0
"""

        let expected = """
module Program

let foo x = 0
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.ForLoopIdentifierIsCamelCase() =
        this.Parse """
module Program

let someFunc() =
    for i = 10 downto 1 do System.Console.Write(i)
"""

        this.AssertNoViolations()

    [<Test>]
    member this.ForLoopIdentifierIsPascalCase() =
        this.Parse """
module program

let someFunc() =
    for I = 10 downto 1 do System.Console.Write(I)
"""

        Assert.IsTrue(this.ViolationExistsAt(5, 8))

    [<Test>]
    member this.ForEachLoopIdentifierIsCamelCase() =
        this.Parse """
module Program

let someFunc() =
    for i in 1..10 do System.Console.Write(i)
"""

        this.AssertNoViolations()

    [<Test>]
    member this.ForEachLoopIdentifierIsPascalCase() =
        this.Parse """
module program

let someFunc() =
    for I in 1..10 do System.Console.Write(I)
"""

        Assert.IsTrue(this.ViolationExistsAt(5, 8))

    [<Test>]
    member this.UnionCaseInBindingContainingPascalCaseValueGeneratesViolation() =
        this.Parse """
module Program

type SingleCaseDU = SingleCaseDU of int

let (SingleCaseDU MyInt) = (SingleCaseDU 5)
"""

        Assert.IsTrue(this.ViolationsExist)

    [<Test>]
    member this.ParameterUnionCaseContainingPascalCaseValueGeneratesViolation() =
        this.Parse """
module Program

type SingleCaseDU = SingleCaseDU of int
let extractInt (SingleCaseDU MyInt) =
    MyInt

let singleCaseDU = SingleCaseDU 5

let result = extractInt singleCaseDU
"""

        Assert.IsTrue(this.ViolationsExist)

    [<Test>]
    member this.PrivateVariableIsCamelCase() =
        this.Parse """
module Program

let private cat = 1
"""

        this.AssertNoViolations()

    [<Test>]
    member this.PrivateVariableIsPascalCase() =
        this.Parse """
module Program

let private Cat = 1
"""

        Assert.IsTrue(this.ViolationExistsAt(4,12))

    [<Test>]
    member this.PublicVariableIsNotReported() =
        this.Parse """
module Program

let Cat = 1
"""

        this.AssertNoViolations()


    [<Test>]
    member this.ExplicitPublicVariableIsNotReported() =
        this.Parse """
module Program

let public Cat = 1
"""

        this.AssertNoViolations()

    [<Test>]
    member this.InternalVariableIsNotReported() =
        this.Parse """
module Program

let internal Cat = 1
"""

        this.AssertNoViolations()
