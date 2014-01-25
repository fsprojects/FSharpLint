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

namespace MattMcveigh.FSharpLint

module LexRules =

    open Microsoft.FSharp.Compiler.SourceCodeServices
    open LexRuleMatching

    let anyOperator =
        {
            CharClass = TokenCharKind.Operator
            ToMatch = Anything
        }

    let delimiter =
        {
            CharClass = TokenCharKind.Delimiter
            ToMatch = Anything
        }

    let spaceCharacter =
        {
            CharClass = TokenCharKind.WhiteSpace
            ToMatch = StringMatch " "
        }

    let operatorWhitespaceRule = [ 
        Not (One (Token spaceCharacter))
        One (Token spaceCharacter)
        One (Token anyOperator)
        One (Token spaceCharacter)
        Not (One (Token spaceCharacter))
    ]

    let delimiterWhitespaceRule = [ 
        One (Token delimiter)
        One (Token spaceCharacter)
        Not (One (Token spaceCharacter))
    ]

    let rules = [
        operatorWhitespaceRule
        delimiterWhitespaceRule
    ]