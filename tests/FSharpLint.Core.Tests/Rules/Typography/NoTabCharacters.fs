module FSharpLint.Core.Tests.Rules.Typography.NoTabCharacters

open System
open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestTypographyTabCharacterInFile() =
    inherit TestNoTabCharactersRuleBase.TestNoTabCharactersRuleBase(NoTabCharacters.rule)

    [<Test>]
    member this.TabCharacterInFile() =
        this.Parse "\t"

        Assert.IsTrue(this.ErrorExistsAt(1, 0))

    [<Test>]
    member this.``Tab character in literal strings are not reported``() =
        let source = String.Format("""
            let a = @"a{0}b"
            let b = {1}
            a{0}b
            {1}
            """, "\t", "\"\"\"")
        this.Parse (source)

        Assert.IsFalse(this.ErrorExistsAt(2, 23))
        Assert.IsFalse(this.ErrorExistsAt(4, 13))
