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

module TestHintMatcher

open System.Linq
open NUnit.Framework
open FParsec
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.HintParser
open FSharpLint.Framework.HintMatcher
open FSharpLint.Framework.LoadVisitors

let generateHintConfig hints =
    Map.ofList 
        [ 
            (AnalyserName, 
                { 
                    Rules = Map.ofList 
                        [ 
                            ("Hints", 
                                { 
                                    Settings = Map.ofList [ ("Hints", Hints(hints)) ] 
                                }) 
                        ]
                    Settings = Map.ofList [] 
                }) 
        ]
    
[<TestFixture>]
type TestHintMatcher() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor getHints))

    [<Test>]
    member this.MatchNotEqualHint() = 
        let config = generateHintConfig ["not (a = b) ===> a <> b"]

        this.Parse("""
module Goat

let (a, b) = (1, 2)
let valid = not (a = b)""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 12))

    [<Test>]
    member this.MatchFunctionApplication() = 
        let config = generateHintConfig ["List.fold (+) 0 x ===> List.sum x"]

        this.Parse("""
module Goat

List.fold (+) 0 x""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchInfixExpression() = 
        let config = generateHintConfig ["4 + 4 ===> 8"]

        this.Parse("""
module Goat

4 + 4""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchPrefixExpression() = 
        let config = generateHintConfig ["4 + %4 ===> 8"]

        this.Parse("""
module Goat

4 + %4""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchAddressOfPrefixExpression() = 
        let config = generateHintConfig ["4 + &4 ===> 8"]

        this.Parse("""
module Goat

4 + &4""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchParenthesesInHintExpression() = 
        let config = generateHintConfig ["6 + (4 / (5)) ===> 8"]

        this.Parse("""
module Goat

6 + 4 / 5""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchParenthesesExpression() = 
        let config = generateHintConfig ["6 + (4 + (5)) ===> 8"]

        this.Parse("""
module Goat

6 + (4 + (5))""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchLambda() = 
        let config = generateHintConfig ["fun x _ y -> x + y ===> 0"]

        this.Parse("""
module Goat

let f = fun x y z -> x + z""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchWildcardLambda() = 
        let config = generateHintConfig ["fun _ -> 1 ===> id"]

        this.Parse("""
module Goat

let f = fun _ -> 1""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchMultipleWildcardLambda() = 
        let config = generateHintConfig ["fun _ _ -> 1 ===> id"]

        this.Parse("""
module Goat

let f = fun _ _ -> 1""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchMultipleWildcardAndVariableLambda() = 
        let config = generateHintConfig ["fun _ a _ b -> 1 ===> id"]

        this.Parse("""
module Goat

let f = fun _ a _ x -> 1""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchIdLambda() = 
        let config = generateHintConfig ["fun x -> x ===> id"]

        this.Parse("""
module Goat

let f = fun x -> x""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.DontMatchIdLambda() = 
        let config = generateHintConfig ["fun x -> x ===> id"]

        this.Parse("""
module Goat

let f = fun x -> 1""", config)

        Assert.IsFalse(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchFunctionApplicationWithBackwardPipe() = 
        let config = generateHintConfig ["(+) 1 x ===> x"]

        this.Parse("""
module Goat

(+) 1 <| 2 + 3""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 1))

    [<Test>]
    member this.MatchFunctionApplicationWithForwardPipe() = 
        let config = generateHintConfig ["List.fold (+) 0 x ===> List.sum x"]

        this.Parse("""
module Goat

[1;2;3] |> List.fold (+) 0""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchMultipleFunctionApplications() = 
        let config = generateHintConfig ["List.head (List.sort x) ===> List.min x"]

        this.Parse("""
module Goat

[1;2;3] |> List.sort |> List.head""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchTupleApplication() = 
        let config = generateHintConfig ["fst (x, y) ===> x"]

        this.Parse("""
module Goat

fst (1, 0) |> ignore""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchListAppendItem() = 
        let config = generateHintConfig ["x::[] ===> [x]"]

        this.Parse("""
module Goat

x::[] |> ignore""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchAppendListToList() = 
        let config = generateHintConfig ["[x]@[y] ===> [x;y]"]

        this.Parse("""
module Goat

[1]@[2] |> ignore""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))