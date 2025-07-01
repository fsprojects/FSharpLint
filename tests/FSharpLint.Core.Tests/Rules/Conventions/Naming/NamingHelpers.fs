module FSharpLint.Core.Tests.Rules.Conventions.NamingHelpers

open NUnit.Framework
open FSharpLint.Rules.Helper.Naming

[<TestFixture>]
type TestConventionsNamingHelpers() =

    [<Test>]
    member _.IsPascalCase() =
        Assert.IsTrue(isPascalCase "DogInBin")

        Assert.IsFalse(isPascalCase "dogInBin")

    [<Test>]
    member _.IsCamelCase() =
        Assert.IsTrue(isCamelCase "dogInBin")

        Assert.IsFalse(isCamelCase "DogInBin")
