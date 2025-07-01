module FSharpLint.Core.Tests.Rules.Conventions.FailwithBadUsage

open System
open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsFailwithBadUsage() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FailwithBadUsage.rule)

    [<Test>]
    member this.FailwithWithBadArgumentsEmptyMessage() =
        this.Parse """
let foo () =
    failwith ""
"""
        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(3, 4))

    [<Test>]
    member this.FailwithWithBadArgumentsEmptyMessage2() =
        this.Parse """
let bar () =
    failwith String.Empty
"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(3, 4))

    [<Test>]
    member this.FailwithWithBadArgumentsEmptyMessage3() =
        this.Parse """
let foo () =
    failwith "foo"
let bar () =
    failwith String.Empty
"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.FailwithWithGoodArguments() =
        this.Parse """
let foo () =
    failwith "foo"
let bar () =
    failwith "bar"
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FailwithWithGoodArguments2() =
        this.Parse """
let foo () =
    failwith "foo"
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FailwithWithBadArgumentsDuplicateExceptionMessage() =
        this.Parse """
let foo () =
    failwith "foobar"
let bar () =
    failwith "foobar"
"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.FailwithShouldNotSwallowExceptions() =
        this.Parse """
try
    foo()
with
| e ->
    failwith "bar"
"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(6, 4))

    [<Test>]
    member this.FailwithWithProperExceptions() =
        this.Parse """
try
    foo()
with
| e ->
    raise new Exception("bar",e)
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.FailwithfShouldNotSwallowExceptions() =
        this.Parse """
try
    foo()
with
| e ->
    failwithf "bar"
"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(6, 4))

    [<Test>]
    member this.FailwithWithfGoodArguments2() =
        this.Parse """
let foo () =
    failwithf "foo"
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FailwithfWithBadArgumentsEmptyMessage3() =
        this.Parse """
let foo () =
    failwithf String.Empty
"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(3, 4))

    [<Test>]
    member this.FailwithfWithNullArgument() =
        this.Parse """
let foo () =
    failwithf null
"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(3, 14))

    [<Test>]
    member this.FailwithfWithFullNameModuleEmptyMessage() =
        this.Parse """
let bar () =
    failwithf System.String.Empty
"""

        Assert.IsTrue this.ErrorsExist
        Assert.IsTrue(this.ErrorExistsAt(3, 4))

    [<Test>]
    member this.FailwithfWithShouldnotFail() =
        this.Parse """
let failIfNone (opt: 'a option) : 'a =
    opt
    |> Option.defaultWith (fun _ -> failwith "A unique error message")
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.ExternDeclarationsShouldnotFail() =
        this.Parse """
[<DllImport("LibFoo")>]
extern bool Foo()

[<DllImport("LibFoo")>]
extern bool Bar()
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.ExternDeclarationsWithoutDllImportShouldnotFail() =
        this.Parse """
extern bool Foo()

extern bool Bar()
"""

        Assert.IsTrue this.NoErrorsExist
