module FSharpLint.Core.Tests.Rules.Conventions.NestedStatements

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Rules.NestedStatements
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsNestedStatements() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(NestedStatements.rule { Config.Depth = 5 })

    [<Test>]
    member this.NestedTooDeep() =
        this.Parse """
module Program

let dog =
    if true then
        if true then
            if true then
                if true then
                    if true then
                        if true then
                            ()
    ()
"""

        Assert.IsTrue(this.ErrorExistsAt(9, 20))

    [<Test>]
    member this.``Repeated nested too deep complains for correct ranges``() =
        this.Parse """
module Program

let dog =
    if true then
        if true then
            if true then
                if true then
                    if true then
                        if true then
                            ()
    if true then
        if true then
            if true then
                if true then
                    if true then
                        if true then
                            ()
    ()
"""

        Assert.IsTrue(this.ErrorExistsAt(9, 20))
        Assert.IsTrue(this.ErrorExistsAt(16, 20))

    [<Test>]
    member this.ElseIfsShouldNotCountAsNested() =
        this.Parse """
module Program

let dog =
    if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else
        ()
"""

        Assert.IsFalse(this.ErrorExistsAt(13, 4))

    [<Test>]
    member this.LambdaWildcardArgumentsMustNotCountAsANestedStatement() =
        this.Parse """
module Program

let dog = (fun _ _ _ _ _ _ _ _ -> ())
"""

        Assert.IsFalse(this.ErrorExistsOnLine(4))

    [<Test>]
    member this.LambdaArgumentsMustNotCountAsANestedStatement() =
        this.Parse """
module Program

let dog = (fun a b c d e f g h i j -> ())
"""

        Assert.IsFalse(this.ErrorExistsOnLine(4))

    [<Test>]
    member this.NestedLambdasCountedCorrectly() =
        this.Parse """
module Program

let dog = (fun x -> fun x -> fun x -> fun x -> fun x -> ())
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 47))


    [<Test>]
    member this.RecordMembersMustNotCountAsNestedStatements() =
        this.Parse """
module Program

 type DummyRecord =
    { Value: int }
    member this.Create1 i = { Value=i }
    member this.Create2 (i: float) = { Value=Convert.ToInt32(i)}
    member this.Create3 (i: float32) = { Value=Convert.ToInt32(i)}
    member this.Create4 (i: bool) = { Value=Convert.ToInt32(i)}
    member this.Create5 (i: string) = { Value=Convert.ToInt32(i)}
    member this.Create6 (i: uint32) = { Value=Convert.ToInt32(i)}
    member this.Create7 (i: uint16) = { Value=Convert.ToInt32(i)}
    member this.Create8 (i: byte) = { Value=Convert.ToInt32(i)}
    member this.Create9 (i: char) = { Value=Convert.ToInt32(i)}
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.NestedStatementsWithinRecordMemberCountedCorrectly() =
        this.Parse """
module Program

type DummyRecord =
    { Value: int }
    member this.DummyMember() =
                if true then
                    if true then
                        if true then
                            if true then
                                if true then
                                    if true then
                                        ()
"""

        Assert.IsTrue(this.ErrorExistsAt(11, 32))