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

module TestFavourIgnoreOverLetWildRules

open NUnit.Framework
open FSharpLint.Rules.FavourIgnoreOverLetWild
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
                        ]
                }) 
        ]

[<TestFixture>]
type TestNameConventionRules() =
    inherit TestRuleBase.TestRuleBase(visitor, config)

    [<Test>]
    member this.LetWildcardUnitValue() = 
        this.Parse """
module Program

let _ = ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetWildcardMultilaneStatementOfUnit() = 
        this.Parse """
module Program

let (_) = 
  let x = 4 + 4
  ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetWildCardInParanUnitValue() = 
        this.Parse """
module Program

let ((((_)))) = List.iter (fun x -> ()) []"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetNonWildcardUnitValue() = 
        this.Parse """
module Program

let a = List.iter (fun x -> ()) []"""

        Assert.IsFalse(this.ErrorExistsAt(4, 4))