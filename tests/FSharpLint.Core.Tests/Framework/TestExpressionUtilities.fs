﻿module FSharpLint.Core.Tests.TestExpressionUtilities

open NUnit.Framework
open FSharp.Compiler.Text
open FSharpLint.Framework.ExpressionUtilities

[<TestFixture>]
type TestExpressionUtilities() =
    [<Test>]
    member _.``TryFindTextOfRange gets expected text from given ranges``() =
        let text = "123\n345\n678"

        let textOfRange (line1, col1) (line2, col2) =
            tryFindTextOfRange (Range.mkRange "" (Position.mkPos line1 col1) (Position.mkPos line2 col2)) text

        Assert.AreEqual(Some "123", textOfRange (1, 0) (1, 3))
        Assert.AreEqual(Some "345", textOfRange (2, 0) (2, 3))
        Assert.AreEqual(Some "678", textOfRange (3, 0) (3, 3))

        Assert.AreEqual(Some "1", textOfRange (1, 0) (1, 1))
        Assert.AreEqual(Some "8", textOfRange (3, 2) (3, 3))

        Assert.AreEqual(Some "123\n345\n678", textOfRange (1, 0) (3, 3))