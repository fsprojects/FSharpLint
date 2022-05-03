module FSharpLint.Core.Tests.Rules.Conventions.AvoidTooShortNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsAvoidTooShortNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AvoidTooShortNames.rule)

    [<Test>]
    member this.AvoidTooShortNamesShouldNotProduceError() =
        this.Parse """
open System

let foo = 1
let bar baz =
    let foobar = "x"
    foobar """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError_1() =
        this.Parse """
open System

let f = 1
let bar baz =
    let foobar = "x"
    foobar """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError_2() =
        this.Parse """
open System

let foo = 1
let b baz n =
    let foobar = "x"
    foobar """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError_3() =
        this.Parse """
open System

let foo = 1
let bar b =
    let foobar = "x"
    foobar """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError_4() =
        this.Parse """
open System

let foo = 1
let bar baz =
    let x = "x"
    x """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AvoidTooShortNamesShouldProduceError_5() =
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
    member this.AvoidTooShortNamesShouldProduceError_6() =
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
