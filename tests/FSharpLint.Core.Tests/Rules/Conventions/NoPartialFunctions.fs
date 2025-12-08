module FSharpLint.Core.Tests.Rules.Conventions.NoPartialFunctions

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsNoPartialFunctions() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(NoPartialFunctions.rule { AdditionalPartials = ["Custom.partial"]; AllowedPartials = ["List.pick"] })

    [<Test>]
    member this.``Violation for partial function which should be replaced with pattern matching``() =
        this.Parse("let x = Option.get None")

        this.AssertViolationWithMessageExists("Consider using pattern matching instead of partial function/method 'Option.get'.")

    [<Test>]
    member this.``Violation for partial function which should be replaced with another function``() =
        this.Parse("let x = List.find 1 [2; 3; 4]")

        this.AssertViolationWithMessageExists("Consider using 'List.tryFind' instead of partial function/method 'List.find'.")

    [<Test>]
    member this.``Quickfix for partial function which should be replaced with another function``() =
        let source = "let x = List.find 1 [2; 3; 4]"
        this.Parse(source)

        let expected = "let x = List.tryFind 1 [2; 3; 4]"
        Assert.AreEqual(expected, this.ApplyQuickFix source)
        this.AssertViolationWithMessageExists( "Consider using 'List.tryFind' instead of partial function/method 'List.find'.")

    [<Test>]
    member this.``Violation for user-specified partial function``() =
        this.Parse("let x = Custom.partial 4")

        this.AssertViolationWithMessageExists("Consider not using partial function 'Custom.partial'.")

    [<Test>]
    member this.``No violation for user-specified allowed partial function``() =
        this.Parse("let x = List.pick id [Some 4; None]")

        this.AssertNoViolations()

    [<Test>]
    member this.``Violation for Option.Value (simple test case)``() =
        this.Parse """
let foo = None
printf foo.Value
"""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue(this.ViolationExistsAt(3, 7))

    [<Test>]
    member this.``Violation for Option.Value (complex test case)``() =
        this.Parse """
module Program =
    let foo = None

    let printFoo() =
        System.Console.WriteLine (foo.Value.ToString())
"""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue(this.ViolationExistsAt(6, 34))
        this.AssertViolationWithMessageExists("Consider using pattern matching instead of partial function/method 'Option.Value'.")

    [<Test>]
    member this.``No violation for calling Value on ref type (regression)``() =
        this.Parse """
module Program =
    let foo = None
    let bar = ref 0

    let printFoo() =
        System.Console.WriteLine (bar.Value.ToString())
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.``Violation for Option.Value (List.tryHead test case)``() =
        this.Parse """
module Program =
    let foo = []

    let printFoo() =
        System.Console.WriteLine ((List.tryHead foo).Value.ToString())
"""

        Assert.IsTrue this.ViolationsExist
        Assert.IsTrue(this.ViolationExistsAt(6, 34))
        this.AssertViolationWithMessageExists("Consider using pattern matching instead of partial function/method 'Option.Value'.")

    [<Test>]
    member this.``No violation for value property in DU``() =
        this.Parse """
module Program

type SomeTypeThatsNotOption =
    | Value of string
    | Count of int

let Foo (foo: SomeTypeThatsNotOption) =
    let foo = SomeTypeThatsNotOption.Value
    ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``No violation for option methods other than Option.Value``() =
        this.Parse """
let foo = None
if foo.IsNone then
    System.Console.WriteLine (foo.ToString())
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``No violation for Map methods that are not Item``() =
        this.Parse """
let foo = Map.empty
if foo.IsEmpty then
    System.Console.WriteLine foo.Count
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``Violation for Map.Item``() =
        this.Parse """
let foo = Map.empty
if foo.Item 1 then
    System.Console.WriteLine foo.Count
"""

        Assert.IsTrue this.ViolationsExist
        this.AssertViolationWithMessageExists("Consider using 'Map.tryFind' instead of partial function/method 'Map.Item'.")

    [<Test>]
    member this.``No violation for List methods that are not Item``() =
        this.Parse """
let foo = List.empty
if foo.IsEmpty then
    System.Console.WriteLine foo.Length
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``Violation for List.Item``() =
        this.Parse """
let foo = List.empty
if foo.Item 1 then
    System.Console.WriteLine foo.Length
"""

        Assert.IsTrue this.ViolationsExist
        this.AssertViolationWithMessageExists("Consider using 'List.tryFind' instead of partial function/method 'List.Item'.")

    [<Test>]
    member this.``Violation for List.Head``() =
        this.Parse """
let foo = List.empty
if foo.Head 1 then
    System.Console.WriteLine foo.Length
"""

        Assert.IsTrue this.ViolationsExist
        this.AssertViolationWithMessageExists("Consider using 'List.tryHead' instead of partial function/method 'List.Head'.")

    [<Test>]
    member this.``Regression found when parsing Console/Program_fs``() =
        this.Parse """
module Program =
    type Foo = Foo of string
    do
        typeof<int>.GetCustomAttributes false
        |> ignore
"""

        this.AssertNoViolations()
(*
    // Examples for future additions, see 'Foo.Bar.Baz' in partialInstanceMemberIdentifiers

    [<Test>]
    member this.``Violation for method invocations to Foo.Bar.Instance.Baz``() =
        this.Parse("
namespace Foo
type Bar() =
    member this.Baz = 'x'
    static member Instance = Bar()
namespace FooBar
module Program =
    let bar = '212'
    Console.WriteLine bar
    let foo = None
    printf foo.ToString()
    System.Console.WriteLine Foo.Bar.Instance.Baz")

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.``Violation text for method invocations to Foo.Bar.Instance.Baz``() =
        this.Parse("
namespace Foo
type Bar() =
    member this.Baz = 'x'
    static member Instance = Bar()
namespace FooBar
module Program =
    System.Console.WriteLine Foo.Bar.Instance.Baz")

        Assert.IsTrue this.ViolationsExist
        this.AssertViolationWithMessageExists("Consider using pattern matching instead of partial function/method 'Foo.Bar.Baz'.")
*)
