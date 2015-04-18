(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

module TestTypographyRules

open NUnit.Framework
open FSharpLint.Rules.Typography
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.LoadVisitors

let setupConfig numberOfSpacesAllowed isOneSpaceAllowedAfterOperator ignoreBlankLines = 
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
                                            ("Length", Length(10))
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
                        ]
                    Settings = Map.ofList []
                })
            ]

let config = setupConfig 0 false false

[<TestFixture>]
type TestNestedStatements() =
    inherit TestRuleBase.TestRuleBase(PlainText(visitor), config)

    [<Test>]
    member this.TooManyCharactersOnLine() = 
        this.Parse "let line = 55 + 77 + 77"

        Assert.IsTrue(this.ErrorExistsAt(1, 11))

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
        this.Parse("fun x -> " + System.Environment.NewLine + "    ()", setupConfig 0 true false)
                        
        Assert.IsFalse(this.ErrorExistsAt(1, 8))

    [<Test>]
    member this.SingleSpaceOnEndOfLineAfterOperatorWithConfigPropertyOff() = 
        this.Parse("fun x -> " + System.Environment.NewLine + "    ()", setupConfig 0 false false)

        Assert.IsTrue(this.ErrorExistsAt(1, 8))

    [<Test>]
    member this.MultipleSpacesOnEndOfLineAfterOperatorWithConfigPropertyOn() = 
        this.Parse("fun x ->  " + System.Environment.NewLine + "    ()", setupConfig 0 true false)
                        
        Assert.IsTrue(this.ErrorExistsAt(1, 8))

    [<Test>]
    member this.NoSpacesOnEndOfLineWithNoSpacesAllowed() = 
        this.Parse("let line = 55", setupConfig 0 false false)
        
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.OneSpaceOnEndOfLineWithNoSpacesAllowed() = 
        this.Parse("let line = 55 ", setupConfig 0 false false)
        
        Assert.IsTrue(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.OneSpaceOnEndOfLineWithOneSpaceAllowed() = 
        this.Parse("let line = 55 ", setupConfig 1 false false)
        
        Assert.IsFalse(this.ErrorExistsAt(1, 14))
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.TwoSpacesOnEndOfLineWithOneSpaceAllowed() = 
        this.Parse("let line = 55  ", setupConfig 1 false false)
        
        Assert.IsTrue(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.TwoSpacesOnEndOfLineWithTwoSpacesAllowed() = 
        this.Parse("let line = 55  ", setupConfig 2 false false)
        
        Assert.IsFalse(this.ErrorExistsAt(1, 15))
        Assert.IsFalse(this.ErrorExistsAt(1, 14))
        Assert.IsFalse(this.ErrorExistsAt(1, 13))

    [<Test>]
    member this.WhitespaceEntireLine() = 
        this.Parse " "

        Assert.IsTrue(this.ErrorExistsAt(1, 0))

    [<Test>]
    member this.WhitespaceEntireLineIgnoreBlankLines() = 
        this.Parse(" ", setupConfig 0 false true)

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
    member this.NewLineOnEndOfFile() = 
        this.Parse ("let dog = 9" + System.Environment.NewLine)

        Assert.IsTrue(this.ErrorExistsAt(2, 0))