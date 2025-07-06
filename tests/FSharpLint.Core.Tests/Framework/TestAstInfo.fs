﻿module FSharpLint.Core.Tests.TestAstInfo

open NUnit.Framework
open FSharpLint.Framework.AstInfo

[<TestFixture>]
type TestAstInfo() =

    [<Test>]
    member _.IsOperator() =
        Assert.IsTrue(isOperator "op_LeftShift")

        Assert.IsTrue(isOperator "op_TwiddleEqualsDivideComma")

        Assert.IsFalse(isOperator "TwiddleEqualsDivideComma")

        Assert.IsFalse(isOperator "op_fTwiddleEqualsDivideComma")

        Assert.IsFalse(isOperator "op_TwiddleEqualsDivideCommaf")