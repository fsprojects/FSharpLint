module FSharpLint.Core.Tests.Rules.Conventions.AsyncExceptionWithoutReturn

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestAsyncExceptionWithoutReturn() =
    inherit FSharpLint.Core.Tests.TestAstNodeRuleBase.TestAstNodeRuleBase(AsyncExceptionWithoutReturn.rule)

    [<Test>]
    member this.RaiseInAsyncBlockWithoutReturn() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        raise (new System.Exception("An error occurred."))
        return true
    }""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.RaiseInAsyncBlockWithReturn() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        return raise (new System.Exception("An error occurred."))
    }""")

        this.AssertNoWarnings()

    [<Test>]
    member this.FailWithInAsyncBlockWithoutReturn() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        failwith "An error occurred."
        return true
    }""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FailwithfInAsyncBlockWithoutReturn1() =
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
    member this.FailwithfInAsyncBlockWithoutReturn2() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        let errCode = 78
        failwithf "Dummy Error Message: %i" errCode
    }""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.FailwithInAsyncBlockWithReturn() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        return failwith "An error occurred."
    }""")

        this.AssertNoWarnings()

    [<Test>]
    member this.FailwithfInAsyncBlockWithReturn() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        let errCode = 78
        return failwithf "Dummy Error Message: %i" errCode
    }""")

        this.AssertNoWarnings()

    [<Test>]
    member this.RaiseInAsyncBlockWithReturnInnerExpression() =
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
    member this.RaiseInAsyncBlockWithoutReturnInnerExpression() =
        this.Parse("""
module Program

let someAsyncFunction () =
    async {
        if 2 = 2 then
            raise (new System.Exception("An error occurred."))

        return true
    }""")

        Assert.IsTrue this.ErrorsExist
