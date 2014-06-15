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

module TestBindingRules

open NUnit.Framework
open FSharpLint.Rules.Binding
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.LoadAnalysers

let config = 
    Map.ofList 
        [ 
            (AnalyserName, 
                { 
                    Rules = Map.ofList 
                        [
                            ("FavourIgnoreOverLetWild", 
                                { 
                                    Settings = Map.ofList 
                                        [ ("Enabled", Enabled(true)) ] 
                                }) 
                            ("UselessBinding", 
                                { 
                                    Settings = Map.ofList 
                                        [ ("Enabled", Enabled(true)) ] 
                                }) 
                        ]
                    Settings = Map.ofList 
                        [
                            ("Enabled", Enabled(true))
                        ]
                }) 
        ]

[<TestFixture>]
type TestBindingRules() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config)

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

    [<Test>]
    member this.UslessBinding() = 
        this.Parse """
module Program

let a = 10
let a = a"""

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.UslessBindingWithParens() = 
        this.Parse """
module Program

let a = 10
let ((a)) = ((a))"""

        Assert.IsTrue(this.ErrorExistsAt(5, 4))