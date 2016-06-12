// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

module TestNestedStatements

open NUnit.Framework
open FSharpLint.Rules.NestedStatements
open FSharpLint.Framework.Configuration

let config = 
    Map.ofList 
        [ 
            (AnalyserName, 
                { 
                    Rules = Map.ofList []
                    Settings = Map.ofList 
                        [ 
                            ("Enabled", Enabled(true)) 
                            ("Depth", Depth(5)) 
                        ]
                })
            ]
 
[<TestFixture>]
type TestNestedStatements() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

    [<Category("Performance")>]
    [<Test>]
    member this.``Performance of nested statements analyser``() = 
        Assert.Less(this.TimeAnalyser(100, defaultConfiguration), 20)

    [<Test>]
    member this.NestedTooDeep() = 
        this.Parse """
module Program

let dog =
    if true then
        if true then
            if true then
                if true then
                    if true then
                        if true then
                            ()
    ()"""

        Assert.IsTrue(this.ErrorExistsAt(9, 20)) 

    [<Test>]
    member this.NestedTooDeepSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("NestedStatements", "*")>]
let dog =
    if true then
        if true then
            if true then
                if true then
                    if true then
                        if true then
                            ()
    ()"""

        Assert.IsFalse(this.ErrorExistsOnLine(10)) 

    [<Test>]
    member this.ElseIfsShouldNotCountAsNested() = 
        this.Parse """
module Program

let dog =
    if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else 
        ()"""

        Assert.IsFalse(this.ErrorExistsAt(13, 4)) 
        
    [<Test>]
    member this.LambdaWildcardArgumentsMustNotCountAsANestedStatement() = 
        this.Parse """
module Program

let dog = (fun _ _ _ _ _ _ _ _ -> ())"""

        Assert.IsFalse(this.ErrorExistsOnLine(4))

    [<Test>]
    member this.LambdaArgumentsMustNotCountAsANestedStatement() = 
        this.Parse """
module Program

let dog = (fun a b c d e f g h i j -> ())"""

        Assert.IsFalse(this.ErrorExistsOnLine(4))

    [<Test>]
    member this.NestedLambdasCountedCorrectly() = 
        this.Parse """
module Program

let dog = (fun x -> fun x -> fun x -> fun x -> fun x -> ())"""

        Assert.IsTrue(this.ErrorExistsAt(4, 47))