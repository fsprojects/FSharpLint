module FSharpLint.Core.Tests.Rules.Conventions.NamingHelpers

open NUnit.Framework
open FSharpLint.Rules.Helper.Naming

[<TestFixture>]
type internal TestConventionsNamingHelpers() =

    [<Test>]
    member __.IsPascalCase() =
        Assert.IsTrue(isPascalCase "DogInBin")

        Assert.IsFalse(isPascalCase "dogInBin")

    [<Test>]
    member __.IsCamelCase() =
        Assert.IsTrue(isCamelCase "dogInBin")

        Assert.IsFalse(isCamelCase "DogInBin")
