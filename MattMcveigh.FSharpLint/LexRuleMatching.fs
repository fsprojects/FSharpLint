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

module LexRuleMatching =

    open System.Text.RegularExpressions
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Tokeniser
    
    /// Provides different methods of matching a token's string.
    type Match =
    | RegexMatch of string
    | StringMatch of string
    | Anything

    /// Provides a way of matching a token by both the token's type and the token's string.
    type MatchToken = 
        {
            CharClass: TokenCharKind
            ToMatch: Match
        }

    /// Finds out if a MatchToken matches a given token.
    let isMatch matchToken token =
        if matchToken.CharClass <> token.Token.CharClass then
            false
        else
            match matchToken.ToMatch with
            | Anything -> true
            | RegexMatch pattern -> Regex.IsMatch(token.FromString, pattern)
            | StringMatch str -> token.FromString = str

    type LexRule =
    | Not of LexRule
    | One of LexRule
    | Optional of LexRule
    | Repetition of LexRule
    | Alternation of LexRule
    | Rule of LexRule list
    | Token of MatchToken

    type Rule =
        {
            Name: string
            Warning: string
            LexRules: LexRule list
        }

    type AnalysisResult =
    | SkipToken
    | NextRule
    | RuleNotMatched

    let analyseRule rule token =
        RuleNotMatched
        
    type ParseResult =
    | Success
    | Failure

    let parser lexRules isSkipTokenFailure (token:Token) =
        match lexRules with
        | rule :: rules -> 
            match analyseRule rule token with
            | SkipToken when isSkipTokenFailure -> Failure, lexRules
            | SkipToken -> Success, lexRules
            | NextRule -> Success, rules
            | RuleNotMatched -> Failure, []
        | [] -> Failure, []

    let parseToken notifyRuleBroken ruleParsers (token:Token) parsers =
        ruleParsers @ parsers 
            |> List.map (fun (parser, rule) -> parser token, rule) 
            |> List.filter (fun ((result, lexRulesLeft), rule) -> 
                if result = Success && lexRulesLeft |> List.isEmpty then
                    notifyRuleBroken rule
                    false
                else
                    result = Success)
            |> List.map (fun ((_, lexRules), rule) -> parser lexRules false, rule)

    let findBrokenRules notifyRuleBroken rules tokens = 
        let ruleParsers = List.map (fun rule -> parser rule.LexRules true, rule) rules

        let rec analyseTokens parsers = function
        | token :: tokens ->
            let parsers = parseToken notifyRuleBroken ruleParsers token parsers
            analyseTokens parsers tokens
        | [] -> ()

        analyseTokens [] tokens