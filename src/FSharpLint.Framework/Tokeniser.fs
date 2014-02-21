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

namespace FSharpLint

/// Contains a lexer (tokeniser function) which given a list of F# lines of code (each element should be a line of code)
/// turns them into a list of tokens, the tokens contain the string matched to the token and a TokenInformation instance.
module Tokeniser =

    open Microsoft.FSharp.Compiler.SourceCodeServices

    /// A single token, contains the string that this token was created from.
    /// e.g. A LET token would contain the string "let".
    type Token =
        {
            Token: TokenInformation
            FromString: string
        }

        override this.ToString() =
            this.Token.TokenName + " (" + this.FromString + ")"

    /// Tokenise a single line of F# code.
    let internal tokeniseLine (sourceTokeniser:SourceTokenizer) (line:string) state =
        let rec tokeniseLine (tokeniser:LineTokenizer) state =
            match tokeniser.ScanToken state with
            | Some token, state ->
                let token = {
                    Token = token
                    FromString = line.Substring(token.LeftColumn, token.FullMatchedLength)
                }
                let tokens, state = tokeniseLine tokeniser state
                token :: tokens, state
            | None, state -> [], state
            
        let tokeniser = sourceTokeniser.CreateLineTokenizer line
        tokeniseLine tokeniser state

    /// Tokenise a list of lines of F# code.
    let rec internal tokeniseLines sourceTokeniser state = function
    | line :: lines ->
        let lineTokens, state = tokeniseLine sourceTokeniser line state
        lineTokens :: tokeniseLines sourceTokeniser state lines
    | [] -> []

    /// Turns a list of F# lines of code into a list of tokens.
    /// The filename provided need not exist.
    let tokenise filename lines =
        let sourceTokeniser = SourceTokenizer([], filename)

        let initalState = 0L
        tokeniseLines sourceTokeniser initalState lines