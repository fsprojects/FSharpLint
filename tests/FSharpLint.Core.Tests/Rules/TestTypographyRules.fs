module TestTypographyRules

open NUnit.Framework
open FSharpLint.Rules.Typography
open FSharpLint.Framework.Configuration

let setupConfig numberOfSpacesAllowed numberOfIndentationSpaces isOneSpaceAllowedAfterOperator ignoreBlankLines = 
    Map.ofList 
        [ 
            (AnalyserName, 
                { 
                    Rules = Map.ofList 
                        [ 
                            ("MaxLinesInFile", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(50)) 
                                        ] 
                                }) 
                            ("MaxCharactersOnLine", 
                                { 
                                    Settings = Map.ofList
                                        [ 
                                            ("Enabled", Enabled(true))
                                            ("Length", Length(40))
                                        ] 
                                }) 
                            ("NoTabCharacters", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true))
                                        ] 
                                }) 
                            ("TrailingNewLineInFile", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true))
                                        ] 
                                }) 
                            ("TrailingWhitespaceOnLine", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true))

                                            ("NumberOfSpacesAllowed", 
                                             NumberOfSpacesAllowed(numberOfSpacesAllowed))

                                            ("OneSpaceAllowedAfterOperator", 
                                             OneSpaceAllowedAfterOperator(isOneSpaceAllowedAfterOperator))

                                            ("IgnoreBlankLines",
                                             IgnoreBlankLines(ignoreBlankLines))
                                        ] 
                                }) 
                            ("Indentation",
                                {
                                    Settings = Map.ofList
                                        [
                                            ("Enabled", Enabled(true)) 

                                            ("NumberOfIndentationSpaces",
                                             NumberOfIndentationSpaces(numberOfIndentationSpaces))
                                        ]
                                })
                        ]
                    Settings = Map.ofList []
                })
            ]

let config = setupConfig 0 4 false false
 
[<TestFixture>]
type TestTypography() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

    [<Category("Performance")>]
    [<Test>]
    member this.``Performance of typography analyser``() = 
        Assert.Less(this.TimeAnalyser(100, defaultConfiguration), 20)

    [<Test>]
    member this.TooManyCharactersOnLine() = 
        this.Parse "let line = 55 + 77 + 77 + 55 + 55 + 55 + 77 + 55 + 55 + 77 + 55 + 55 + 77 + 77"

        Assert.IsTrue(this.ErrorExistsAt(1, 41))

    [<Test>]
    member this.TooManyCharactersOnLineSuppressed() = 
        this.Parse """
        [<SuppressMessage("Typography", "MaxCharactersOnLine")>]
        let line = 55 + 77 + 77"""

        Assert.IsFalse(this.ErrorExistsAt(3, 11))

    [<Test>]
    member this.TooManyLinesInFile() = 
        this.Parse (String.replicate 50 System.Environment.NewLine)
        
        Assert.IsTrue(this.ErrorExistsAt(51, 0))

    [<Test>]
    member this.WhitespaceOnEndOfLine() = 
        this.Parse "let line = 55 "

        Assert.IsTrue(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.WhitespaceOnEndOfLineSuppressed() = 
        this.Parse """
        [<SuppressMessage("Typography", "TrailingWhitespaceOnLine")>]
        module Dog =
            let line = 55 """

        Assert.IsFalse(this.ErrorExistsAt(4, 25))

    [<Test>]
    member this.SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOn() = 
        this.Parse("fun x -> " + System.Environment.NewLine + "    ()", setupConfig 0 4 true false)
                        
        Assert.IsFalse(this.ErrorExistsAt(1, 8))

    [<Test>]
    member this.SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOff() = 
        this.Parse("fun x -> " + System.Environment.NewLine + "    ()", setupConfig 0 4 false false)

        Assert.IsTrue(this.ErrorExistsAt(1, 8))

    [<Test>]
    member this.MultipleSpacesOnEndOfLineAfterOperatorWithConfigPropertyOn() = 
        this.Parse("fun x ->  " + System.Environment.NewLine + "    ()", setupConfig 0 4 true false)
                        
        Assert.IsTrue(this.ErrorExistsAt(1, 8))

    [<Test>]
    member this.NoSpacesOnEndOfLineWithNoSpacesAllowed() = 
        this.Parse("let line = 55", setupConfig 0 4 false false)
        
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.OneSpaceOnEndOfLineWithNoSpacesAllowed() = 
        this.Parse("let line = 55 ", setupConfig 0 4 false false)
        
        Assert.IsTrue(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.OneSpaceOnEndOfLineWithOneSpaceAllowed() = 
        this.Parse("let line = 55 ", setupConfig 1 4 false false)
        
        Assert.IsFalse(this.ErrorExistsAt(1, 14))
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.TwoSpacesOnEndOfLineWithOneSpaceAllowed() = 
        this.Parse("let line = 55  ", setupConfig 1 4 false false)
        
        Assert.IsTrue(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.TwoSpacesOnEndOfLineWithTwoSpacesAllowed() = 
        this.Parse("let line = 55  ", setupConfig 2 4 false false)
        
        Assert.IsFalse(this.ErrorExistsAt(1, 15))
        Assert.IsFalse(this.ErrorExistsAt(1, 14))
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.WhitespaceEntireLine() = 
        this.Parse " "

        Assert.IsTrue(this.ErrorExistsAt(1, 0))

    [<Test>]
    member this.WhitespaceEntireLineIgnoreBlankLines() = 
        this.Parse(" ", setupConfig 0 1 false true)

        Assert.IsFalse(this.ErrorExistsAt(1, 0))

    [<Test>]
    member this.WhitespaceOnEndOfLineAfterNewLine() = 
        this.Parse (System.Environment.NewLine + "let d = 0 ")
        
        Assert.IsTrue(this.ErrorExistsAt(2, 9))

    [<Test>]
    member this.TabCharacterInFile() = 
        this.Parse "\t"

        Assert.IsTrue(this.ErrorExistsAt(1, 0))

    [<Test>]
    member this.TabCharacterInFileSuppressed() = 
        this.Parse (sprintf """
        [<SuppressMessage("Typography", "NoTabCharacters")>]
        %slet foo = true""" "\t")

        Assert.IsFalse(this.ErrorExistsAt(3, 8))

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
    member this.NewLineOnEndOfFile() =
        this.Parse ("let dog = 9" + System.Environment.NewLine)

        Assert.IsTrue(this.ErrorExistsAt(2, 0))

    [<Test>]
    member this.``Error for incorrect indentation``() =
        this.Parse """
module P

let x =
  x"""

        Assert.IsTrue(this.ErrorExistsAt(5, 0))

    [<Test>]
    member this.``No error for correct indentation``() =
        this.Parse """
module P

let x =
    x"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for incorrect record field indentation``() =
        this.Parse """
module P

let rainbow =
    { X = "X"
          Y = "Y"}"""

        Assert.IsTrue(this.ErrorExistsAt(6, 0))

    [<Test>]
    member this.``No error for correct record field indentation``() =
        this.Parse """
module P

let rainbow =
    { X = "X"
      Y = "Y"}"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``No error for correct array member indentation``() =
        this.Parse """
module P

let pascalsTriangle =
    [| 1
       2
       3
    |]"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``No error for correct list member indentation``() =
        this.Parse """
module P

let pascalsTriangle =
    [ 1
      2
      3
    ]"""

        Assert.IsTrue(this.NoErrorsExist)