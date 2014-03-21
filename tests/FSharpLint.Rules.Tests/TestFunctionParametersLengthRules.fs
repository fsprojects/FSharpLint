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

module TestFunctionParametersLengthRules

open NUnit.Framework
open FSharpLint.Rules.FunctionParametersLength
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
                            ("MaxParameters", MaxParameters(5)) 
                        ]
                }) 
        ]

[<TestFixture>]
type TestNameConventionRules() =
    inherit TestRuleBase.TestRuleBase(visitor, config)

    [<Test>]
    member this.FiveParameters() = 
        this.Parse """
module Program

let foo one two three four five = ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 27))

    [<Test>]
    member this.FourParameters() = 
        this.Parse """
module Program

let foo one two three four = ()"""

        Assert.IsFalse(this.ErrorExistsAt(4, 23))