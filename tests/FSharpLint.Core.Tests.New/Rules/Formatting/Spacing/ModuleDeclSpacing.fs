module FSharpLint.Core.Tests.Rules.Formatting.ModuleDeclSpacing

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestFormattingModuleDeclSpacing() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ModuleDeclSpacing.rule)
    
    [<Test>]
    member this.``Error for missing space between module declarations``() =
        this.Parse """
module Program

let x = 1

let y = 2
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 0))

    [<Test>]
    member this.``Error for no space between module declarations``() =
        this.Parse """
module Program

let x = 1
let y = 2
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 0))

    [<Test>]
    member this.``No error for correct spacing between module declarations``() =
        this.Parse """
module Program

let x = 1


let y = 2
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``No error for correct spacing between module declarations with comments``() =
        this.Parse """
module Program

/// Comment 1
let x = 1


/// Comment 2
let y = 2
"""

        Assert.IsTrue(this.NoErrorsExist)
