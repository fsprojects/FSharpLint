module FSharpLint.Core.Tests.Rules.Conventions.AsyncExceptionWithoutReturn

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestAsyncExceptionWithoutReturn() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AsyncExceptionWithoutReturn.rule)

    [<Test>]
    member this.abcdefg1() = 
        this.Parse("""
namespace Program

let someAsyncFunction = async {
    raise (new System.Exception("An error occurred."))
    true
    }""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.abcdefg2() = 
        this.Parse("""
namespace Program

let someAsyncFunction = async {
    return raise (new System.Exception("An error occurred."))
    }""")

        this.AssertNoWarnings()
