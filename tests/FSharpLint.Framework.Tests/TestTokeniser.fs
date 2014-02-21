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

module TestTokeniser

open NUnit.Framework
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Tokeniser

type Assert with
    static member AreTokensEqual(expectedTokens, (tokens:Token list)) =
        let zippedLists = List.zip expectedTokens tokens

        List.iter (fun (expectedToken, actualToken) -> 
            Assert.AreEqual(expectedToken, actualToken.Token.CharClass)) zippedLists
end

[<TestFixture>]
type TestTokeniser() =
    [<Test>]
    member self.TokeniseLine() = 
        let expectedTokens = [
            TokenCharKind.Keyword
            TokenCharKind.WhiteSpace
            TokenCharKind.Identifier
            TokenCharKind.WhiteSpace
            TokenCharKind.Operator
            TokenCharKind.WhiteSpace
            TokenCharKind.Literal
            TokenCharKind.WhiteSpace
            TokenCharKind.Operator
            TokenCharKind.WhiteSpace
            TokenCharKind.Literal
        ]

        let actualTokens = tokenise "" ["let meow = 5 + 6"]

        Assert.AreTokensEqual(expectedTokens, actualTokens.Head)

    [<Test>]
    member self.TokeniseLines() = 
        let expectedTokensLine1 = [
            TokenCharKind.Keyword
            TokenCharKind.WhiteSpace
            TokenCharKind.Identifier
            TokenCharKind.WhiteSpace
            TokenCharKind.Operator
            TokenCharKind.WhiteSpace
            TokenCharKind.Literal
        ]

        let expectedTokensLine2 = [
            TokenCharKind.Keyword
            TokenCharKind.WhiteSpace
            TokenCharKind.Identifier
            TokenCharKind.Operator
            TokenCharKind.Delimiter
            TokenCharKind.Delimiter
        ]

        let actualTokens = tokenise "" ["let meow = 5";"let dog=()"]

        Assert.AreTokensEqual(expectedTokensLine1, actualTokens.Head)
        Assert.AreTokensEqual(expectedTokensLine2, actualTokens.Tail.Head)

    [<Test>]
    member self.TokeniseLineWithNewLineInside() = 
        let expectedTokens = [
            TokenCharKind.Keyword
            TokenCharKind.WhiteSpace
            TokenCharKind.WhiteSpace
            TokenCharKind.Identifier
            TokenCharKind.WhiteSpace
            TokenCharKind.Operator
            TokenCharKind.WhiteSpace
            TokenCharKind.Literal
        ]

        let actualTokens = tokenise "" ["let \nmeow = 5"]

        Assert.AreTokensEqual(expectedTokens, actualTokens.Head)

    [<Test>]
    member self.TokeniseInvalidCode() = 
        let expectedTokens = [
            TokenCharKind.Keyword
            TokenCharKind.WhiteSpace
            TokenCharKind.LineComment
        ]

        let actualTokens = tokenise "" ["let ////"]

        Assert.AreTokensEqual(expectedTokens, actualTokens.Head)

    [<Test>]
    member self.TokeniseNoLines() = 
        let actualTokens = tokenise "" []

        Assert.AreEqual([], actualTokens)