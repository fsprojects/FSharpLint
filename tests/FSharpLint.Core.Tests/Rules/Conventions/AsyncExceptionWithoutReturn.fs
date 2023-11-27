﻿module FSharpLint.Core.Tests.Rules.Conventions.AsyncExceptionWithoutReturn

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestAsyncExceptionWithoutReturn() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(AsyncExceptionWithoutReturn.rule)

    [<Test>]
    member this.AsyncExceptionWithoutReturn() = 
        this.Parse("""
namespace Program

let someAsyncFunction = async {
    raise (new System.Exception("An error occurred."))
    return true
    }""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AsyncExceptionWithoutReturn_2() = 
        this.Parse("""
namespace Program

let someAsyncFunction = async {
    return raise (new System.Exception("An error occurred."))
    }""")

        this.AssertNoWarnings()

    [<Test>]
    member this.AsyncExceptionWithoutReturnOnFailWith() = 
        for ident in ["failwith"; "failwithf"] do
            
            this.Parse(sprintf """
namespace Program

let someAsyncFunction = async {
    %s "An error occurred."
    return true
    }""" ident)

            Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.AsyncExceptionWithoutReturn_4() = 
    member this.AsyncExceptionWithoutReturnOnFailWith_2() = 
        for ident in ["failwith"; "failwithf"] do
            this.Parse(sprintf """
namespace Program

let someAsyncFunction = async {
    return %s "An error occurred."
    }""" ident)

        this.AssertNoWarnings()
