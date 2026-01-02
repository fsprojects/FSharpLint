module FSharpLint.Core.Tests.Rules.Conventions.AvoidTooShortNaming

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsAvoidTooShortNaming() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidTooShortNaming.rule)

    [<Test>]
    member this.ShouldNotProduceError1() =
        this.Parse """
module Program

let foo = 1
let bar baz =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.ShouldProduceError2() =
        this.Parse """
module Program

let f = 1
let bar baz =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ShouldProduceError3() =
        this.Parse """
module Program

let foo = 1
let b baz n =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ShouldProduceError4() =
        this.Parse """
module Program

let foo = 1
let bar b =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ShouldProduceError5() =
        this.Parse """
module Program

let foo = 1
let bar baz =
    let x = "x"
    x
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ShouldProduceError6() =
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
    member this.ShouldProduceError7() =
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
    member this.ShouldProduceError8() =
        this.Parse """
type Foo<'T> = Option<'T>
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ShouldProduceError9() =
        this.Parse """
type Foo<'SomeType> = Option<'SomeType>
"""
        this.AssertNoWarnings()

    [<Test>]
    member this.ShouldProduceError10() =
        this.Parse """
let Foo (x: int) =
    x.ToString()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ShouldProduceError11() =
        this.Parse """
match foo with
| x -> ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ShouldProduceError12() =
        this.Parse """
match foo with
| Some(x) -> ()
"""
        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ShouldProduceError13() =
        this.Parse """
match foo with
| Some(x) -> (x)
"""
        Assert.IsTrue(this.ErrorExistsAt(3, 7))
        Assert.IsFalse(this.ErrorExistsAt(3, 14))

    [<Test>]
    member this.ShouldProduceError14() =
        this.Parse """
async {
    let! z = async { return 1 + 2 }
    return z
} |> Async.RunSynchronously |> ignore<int>
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ShouldProduceError15() =
        this.Parse """
async {
    let! result = async { return 1 + 2 }
    return result
} |> Async.RunSynchronously |> ignore<int>
"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.ShouldProduceError16() =
        this.Parse """
type SomeDU =
    | SomeMember of int * string * bool
    | SomeOtherMember of int

let fooFunction (arg: SomeDU) =
    match arg with
    | SomeDU.SomeMember(x, _, _) -> x
    | SomeDU.SomeOtherMember theInt -> theInt
"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.ShouldProduceError17() =
        this.Parse """
fun x -> x + 1 |> ignore
"""

        Assert.IsTrue this.ErrorsExist
