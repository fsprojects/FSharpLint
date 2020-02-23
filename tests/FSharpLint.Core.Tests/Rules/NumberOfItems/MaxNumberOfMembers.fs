module FSharpLint.Core.Tests.Rules.NumberOfItems.MaxNumberOfMembers

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestMaxNumberOfMembers() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxNumberOfMembers.rule { maxItems = 5 })

    [<Test>]
    member this.SixClassProperties() =
        this.Parse """
module Program

type Test() =
    member val One = 0 with get, set
    member val Two = 0 with get, set
    member val Three = 0 with get, set
    member val Four = 0 with get, set
    member val Five = 0 with get, set
    member val Six = 0 with get, set"""

        Assert.IsTrue(this.ErrorExistsAt(10, 4))

    [<Test>]
    member this.FiveClassProperties() =
        this.Parse """
module Program

type Test() =
    member val One = 0 with get, set
    member val Two = 0 with get, set
    member val Three = 0 with get, set
    member val Four = 0 with get, set
    member val Five = 0 with get, set"""

        this.AssertNoWarnings()

    [<Test>]
    member this.SixClassAbstractMethods() =
        this.Parse """
module Program

type Test() =
    abstract member One: unit -> unit
    abstract member Two: unit -> unit
    abstract member Three: unit -> unit
    abstract member Four: unit -> unit
    abstract member Five: unit -> unit
    abstract member Six: unit -> unit"""

        Assert.IsTrue(this.ErrorExistsAt(10, 4))

    [<Test>]
    member this.FiveClassAbstractMethods() =
        this.Parse """
module Program

type Test() =
    abstract member One: unit -> unit
    abstract member Two: unit -> unit
    abstract member Three: unit -> unit
    abstract member Four: unit -> unit
    abstract member Five: unit -> unit"""

        this.AssertNoWarnings()

    [<Test>]
    member this.SixClassMethods() =
        this.Parse """
module Program

type Test() =
    member this.One() = ()
    member this.Two() = ()
    member this.Three() = ()
    member this.Four() = ()
    member this.Five() = ()
    member this.Six() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(10, 4))

    [<Test>]
    member this.SixClassMethodsLastPrivate() =
        this.Parse """
module Program

type Test() =
    member this.One() = ()
    member this.Two() = ()
    member this.Three() = ()
    member this.Four() = ()
    member this.Five() = ()
    private member this.Six() = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FiveClassMethods() =
        this.Parse """
module Program

type Test() =
    member this.One() = ()
    member this.Two() = ()
    member this.Three() = ()
    member this.Four() = ()
    member this.Five() = ()"""

        this.AssertNoWarnings()
