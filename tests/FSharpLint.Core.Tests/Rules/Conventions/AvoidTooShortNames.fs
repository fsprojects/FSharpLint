module FSharpLint.Core.Tests.Rules.Conventions.AvoidTooShortNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsAvoidTooShortNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidTooShortNames.rule)

    [<Test>]
    member this.AvoidTooShortNamesShouldNotProduceViolation1() =
        this.Parse """
module Program

let foo = 1
let bar baz =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation2() =
        this.Parse """
module Program

let f = 1
let bar baz =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation3() =
        this.Parse """
module Program

let foo = 1
let b baz n =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation4() =
        this.Parse """
module Program

let foo = 1
let bar b =
    let foobar = "x"
    foobar
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation5() =
        this.Parse """
module Program

let foo = 1
let bar baz =
    let x = "x"
    x
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation6() =
        this.Parse """
type CellCreatedFast =
    private
        {
            Y: array<byte>
            DerivativeKeyData: array<byte>
        }
 """

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation7() =
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

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation8() =
        this.Parse """
type Foo<'T> = Option<'T>
"""
        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation9() =
        this.Parse """
type Foo<'SomeType> = Option<'SomeType>
"""
        this.AssertNoViolations()

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation10() =
        this.Parse """
let Foo (x: int) =
    x.ToString()
"""
        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation11() =
        this.Parse """
match foo with
| x -> ()
"""
        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation12() =
        this.Parse """
match foo with
| Some(x) -> ()
"""
        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation13() =
        this.Parse """
match foo with
| Some(x) -> (x)
"""
        Assert.IsTrue(this.ViolationExistsAt(3, 7))
        Assert.IsFalse(this.ViolationExistsAt(3, 14))

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation14() =
        this.Parse """
async {
    let! z = async { return 1 + 2 }
    return z
} |> Async.RunSynchronously |> ignore<int>
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation15() =
        this.Parse """
async {
    let! result = async { return 1 + 2 }
    return result
} |> Async.RunSynchronously |> ignore<int>
"""

        Assert.IsTrue this.NoViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation16() =
        this.Parse """
type SomeDU =
    | SomeMember of int * string * bool
    | SomeOtherMember of int

let fooFunction (arg: SomeDU) =
    match arg with
    | SomeDU.SomeMember(x, _, _) -> x
    | SomeDU.SomeOtherMember theInt -> theInt
"""

        Assert.IsTrue this.ViolationsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceViolation17() =
        this.Parse """
fun x -> x + 1 |> ignore
"""

        Assert.IsTrue this.ViolationsExist
