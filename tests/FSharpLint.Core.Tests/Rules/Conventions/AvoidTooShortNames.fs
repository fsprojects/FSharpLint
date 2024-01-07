module FSharpLint.Core.Tests.Rules.Conventions.AvoidTooShortNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsAvoidTooShortNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidTooShortNames.rule)

    [<Test>]
    member this.AvoidTooShortNamesShouldNotProduceError1() =
        this.Parse """
open System

let foo = 1
let bar baz =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError2() =
        this.Parse """
open System

let f = 1
let bar baz =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError3() =
        this.Parse """
open System

let foo = 1
let b baz n =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError4() =
        this.Parse """
open System

let foo = 1
let bar b =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError5() =
        this.Parse """
open System

let foo = 1
let bar baz =
    let x = "x"
    x
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError6() =
        this.Parse """
type CellCreatedFast =
    private
        {
            Y: array<byte>
            DerivativeKeyData: array<byte>
        }
 """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError7() =
        this.Parse """
type TorStreamCipher(keyBytes: array<byte>, ivOpt: Option<array<byte>>) =
    member self.Encrypt(data: array<byte>) : array<byte> =
        let rec innerEncrypt (x: int) (state: array<byte>) =
            if x >= data.Length then
                state
            else
                Array.empty
        innerEncrypt 3 keyBytes
 """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError8() =
        this.Parse """
type Foo<'T> = Option<'T>
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError9() =
        this.Parse """
type Foo<'SomeType> = Option<'SomeType>
"""
        this.AssertNoWarnings()

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError10() =
        this.Parse """
let Foo (x: int) =
    x.ToString()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError11() =
        this.Parse """
match foo with
| x -> ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError12() =
        this.Parse """
match foo with
| Some(x) -> ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError13() =
        this.Parse """
match foo with
| Some(x) -> (x)
"""
        Assert.IsTrue(this.ErrorExistsAt(3, 7))
        Assert.IsFalse(this.ErrorExistsAt(3, 14))

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError14() =
        this.Parse """
async {
    let! z = async { return 1 + 2 }
    return z
} |> Async.RunSynchronously |> ignore<int>
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError15() =
        this.Parse """
async {
    let! result = async { return 1 + 2 }
    return result
} |> Async.RunSynchronously |> ignore<int>
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError16() =
        this.Parse """
type SomeDU =
    | SomeMember of int * string * bool

let fooFunction (arg: SomeDU) =
    match arg with
    | SomeDU.SomeMember(x, _, _) -> x
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError17() =
        this.Parse """
fun x -> x + 1 |> ignore
"""

        Assert.IsTrue this.ErrorsExist
