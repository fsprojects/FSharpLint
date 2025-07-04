module FSharpLint.Core.Tests.Rules.Typography.NoTabCharacters

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
        this.Parse (sprintf """
            let a = @"a%sb"
            let b = %s
            a%sb
            %s
            """ "\t" "\"\"\"" "\t" "\"\"\"")

        Assert.IsFalse(this.ErrorExistsAt(2, 23))
        Assert.IsFalse(this.ErrorExistsAt(4, 13))
