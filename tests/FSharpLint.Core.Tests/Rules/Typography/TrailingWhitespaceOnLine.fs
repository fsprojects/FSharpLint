module FSharpLint.Core.Tests.Rules.Typography.TrailingWhitespaceOnLine

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOn() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { numberOfSpacesAllowed = 0; oneSpaceAllowedAfterOperator = true; ignoreBlankLines = false })
    
    [<Test>]
    member this.SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOn() = 
        this.Parse("fun x -> " + System.Environment.NewLine + "    ()")
                        
        Assert.IsFalse(this.ErrorExistsAt(1, 8))
        
[<TestFixture>]
type SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOff() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { numberOfSpacesAllowed = 0; oneSpaceAllowedAfterOperator = false; ignoreBlankLines = false })
    
    [<Test>]
    member this.SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOff() = 
        this.Parse("fun x -> " + System.Environment.NewLine + "    ()")

        Assert.IsTrue(this.ErrorExistsAt(1, 8))
        
    [<Test>]
    member this.NoSpacesOnEndOfLineWithNoSpacesAllowed() = 
        this.Parse("let line = 55")
        
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.OneSpaceOnEndOfLineWithNoSpacesAllowed() = 
        this.Parse("let line = 55 ")
        
        Assert.IsTrue(this.ErrorExistsAt(1, 13))
        
    [<Test>]
    member this.WhitespaceOnEndOfLineAfterNewLine() = 
        this.Parse (System.Environment.NewLine + "let d = 0 ")
        
        Assert.IsTrue(this.ErrorExistsAt(2, 9))       
       

[<TestFixture>]
type MultipleSpacesOnEndOfLineAfterOperatorWithConfigPropertyOn() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { numberOfSpacesAllowed = 0; oneSpaceAllowedAfterOperator = true; ignoreBlankLines = false })
    
    [<Test>]
    member this.MultipleSpacesOnEndOfLineAfterOperatorWithConfigPropertyOn() = 
        this.Parse("fun x ->  " + System.Environment.NewLine + "    ()")
                        
        Assert.IsTrue(this.ErrorExistsAt(1, 8))

[<TestFixture>]
type TestOneSpace() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { numberOfSpacesAllowed = 1; oneSpaceAllowedAfterOperator = false; ignoreBlankLines = false })
    
    [<Test>]
    member this.OneSpaceOnEndOfLineWithOneSpaceAllowed() = 
        this.Parse("let line = 55 ")
        
        Assert.IsFalse(this.ErrorExistsAt(1, 14))
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.TwoSpacesOnEndOfLineWithOneSpaceAllowed() = 
        this.Parse("let line = 55  ")
        
        Assert.IsTrue(this.ErrorExistsAt(1, 13))
        
[<TestFixture>]
type TwoSpacesOnEndOfLineWithTwoSpacesAllowed() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { numberOfSpacesAllowed = 2; oneSpaceAllowedAfterOperator = false; ignoreBlankLines = false })

    [<Test>]
    member this.TwoSpacesOnEndOfLineWithTwoSpacesAllowed() = 
        this.Parse("let line = 55  ")
        
        Assert.IsFalse(this.ErrorExistsAt(1, 15))
        Assert.IsFalse(this.ErrorExistsAt(1, 14))
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

[<TestFixture>]
type WhitespaceEntireLine() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { numberOfSpacesAllowed = 0; oneSpaceAllowedAfterOperator = false; ignoreBlankLines = false })    
    
    [<Test>]
    member this.WhitespaceEntireLine() = 
        this.Parse " "

        Assert.IsTrue(this.ErrorExistsAt(1, 0))

[<TestFixture>]
type WhitespaceEntireLineIgnoreBlankLines() =
    inherit TestLineRuleBase.TestLineRuleBase(TrailingWhitespaceOnLine.rule { numberOfSpacesAllowed = 0; oneSpaceAllowedAfterOperator = false; ignoreBlankLines = true })
    
    [<Test>]
    member this.WhitespaceEntireLineIgnoreBlankLines() = 
        this.Parse(" ")

        Assert.IsFalse(this.ErrorExistsAt(1, 0))