module FSharpLint.Core.Tests.Rules.Typography.NoTabCharacters

open NUnit.Framework
open FSharpLint.Rules

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

    [<Test>]
    member this.TabCharacterInFileSuggestedFix() =
        let source = "\t"
        let expected = String.replicate 4 " "
        this.Parse source

        Assert.IsTrue(this.ErrorExistsAt(1, 0))

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

