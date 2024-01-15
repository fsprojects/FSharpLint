module FSharpLint.Core.Tests.Rules.Binding.TupleOfWildcards

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestBindingTupleOfWildcards() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(TupleOfWildcards.rule)

    [<Test>]
    member this.TupleOfWildcards() = 
        this.Parse """
module Program

type Cat = | Persian of int * int

match Persian(1, 3) with
| Persian(_, _) -> ()
"""

        Assert.IsTrue(this.ErrorExistsAt(7, 10))

    [<Test>]
    member this.``Method's parameter list of wildcards should not be treated as tuple of wildcards.``() = 
        this.Parse """
module Program

type Cat() = 
    member __.Persian(_, _) = ()
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Constructor's parameter list of wildcards should not be treated as tuple of wildcards.``() = 
        this.Parse """
module Program

type Cat() = 
    new(_, _) = Cat()

    member __.Persian(_) = ()
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Method with type argument's parameter list of wildcards should not be treated as tuple of wildcards.``() = 
        this.Parse """
module Program

type Cat() = 
    member __.Persian<'t>(_, _) = ()
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Method's parameter list of wildcards in object expressions should not be treated as tuple of wildcards.``() =
        this.Parse """
module Program

type I =
    abstract member Two : bool * bool -> bool

let x =
    { new I with
        member __.Two(_, _) = false }
"""

        Assert.IsFalse(this.ErrorsExist)
