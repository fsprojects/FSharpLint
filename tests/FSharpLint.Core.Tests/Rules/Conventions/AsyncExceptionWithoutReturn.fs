module FSharpLint.Core.Tests.Rules.Conventions.AsyncExceptionWithoutReturn

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestAsyncExceptionWithoutReturn() =
    inherit FSharpLint.Core.Tests.TestAstNodeRuleBase.TestAstNodeRuleBase(AsyncExceptionWithoutReturn.rule)

    [<Test>]
    member this.AsyncRaiseWithoutReturn() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        raise (new System.Exception("An error occurred."))
        return true
    }""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AsyncRaiseWithReturn() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        return raise (new System.Exception("An error occurred."))
    }""")

        this.AssertNoWarnings()

    [<Test>]
    member this.AsyncFailWithWithoutReturn() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        failwith "An error occurred."
        return true
    }""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AsyncFailwithfWithoutReturn1() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        let errCode = 78
        failwithf "Dummy Error Message: %i" errCode
        return true
    }""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AsyncFailwithfWithoutReturn2() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        let errCode = 78
        failwithf "Dummy Error Message: %i" errCode
    }""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AsyncFailwithWithReturn() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        return failwith "An error occurred."
    }""")

        this.AssertNoWarnings()

    [<Test>]
    member this.AsyncFailwithfWithReturn() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        let errCode = 78
        return failwithf "Dummy Error Message: %i" errCode
    }""")

        this.AssertNoWarnings()

    [<Test>]
    member this.AsyncRaiseWithReturnInnerExpression() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        if 2 = 2 then
            return raise (new System.Exception("An error occurred."))

        return true
    }""")

        this.AssertNoWarnings()

    [<Test>]
    member this.AsyncRaiseWithoutReturnInnerExpression() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        if 2 = 2 then
            raise (new System.Exception("An error occurred."))

        return true
    }""")

        Assert.IsTrue this.ErrorsExist
