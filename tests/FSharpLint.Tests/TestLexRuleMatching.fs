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

module TestLexRuleMatching

open NUnit.Framework
open Microsoft.FSharp.Compiler.SourceCodeServices
open MattMcveigh.FSharpLint.LexRuleMatching
open MattMcveigh.FSharpLint.Tokeniser

[<TestFixture>]
type TestLexRuleMatching() =
    let stubToken =
        {
            LeftColumn = 9
            RightColumn = 10
            ColorClass = TokenColorKind.Identifier
            CharClass = TokenCharKind.Identifier
            TriggerClass = TriggerClass.MatchBraces
            Tag = 4
            TokenName = ""
            FullMatchedLength = 10
        }

    [<Test>]
    member self.IsMatchAnything() = 
        let token = {
            Token = stubToken
            FromString = "meow"
        }

        let matchToken = { CharClass = TokenCharKind.Identifier; ToMatch = Anything }

        Assert.IsTrue(isMatch matchToken token)

    [<Test>]
    member self.IsNotMatchAnything() = 
        let token = {
            Token = stubToken
            FromString = "meow"
        }

        let matchToken = { CharClass = TokenCharKind.Literal; ToMatch = Anything }

        Assert.IsFalse(isMatch matchToken token)

    [<Test>]
    member self.IsMatchString() = 
        let token = {
            Token = stubToken
            FromString = "meow"
        }

        let matchToken = { CharClass = TokenCharKind.Identifier; ToMatch = StringMatch "meow" }

        Assert.IsTrue(isMatch matchToken token)

    [<Test>]
    member self.IsNotMatchString() = 
        let token = {
            Token = stubToken
            FromString = "meow"
        }

        let matchToken = { CharClass = TokenCharKind.Identifier; ToMatch = StringMatch "dog" }

        Assert.IsFalse(isMatch matchToken token)

    [<Test>]
    member self.IsMatchPattern() = 
        let token = {
            Token = stubToken
            FromString = "meow"
        }

        let matchToken = { CharClass = TokenCharKind.Identifier; ToMatch = RegexMatch ".{3}w" }

        Assert.IsTrue(isMatch matchToken token)

    [<Test>]
    member self.IsNotMatchPattern() = 
        let token = {
            Token = stubToken
            FromString = "meow"
        }

        let matchToken = { CharClass = TokenCharKind.Identifier; ToMatch = RegexMatch ".{4}w" }

        Assert.IsFalse(isMatch matchToken token)