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
open FSharpLint.Framework.LoadVisitors

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
                            ("WildcardNamedWithAsPattern", 
                                { 
                                    Settings = Map.ofList 
                                        [ ("Enabled", Enabled(true)) ] 
                                }) 
                            ("TupleOfWildcards", 
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
    member this.LetWildcardUnitValueSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Binding", "FavourIgnoreOverLetWild")>]
let _ = ()"""

        Assert.IsFalse(this.ErrorExistsOnLine(5))

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
    member this.NotUslessBindingAsShadowingMutableWithImmutable() = 
        this.Parse """
module Program

let mutable a = 10
let a = a"""

        Assert.IsFalse(this.ErrorExistsOnLine(5))
        ()

    [<Test>]
    member this.NotUslessBindingAsShadowingImmutableWithMutable() = 
        this.Parse """
module Program

let a = 10
let mutable a = a"""

        Assert.IsFalse(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.UslessBindingSuppressed() = 
        this.Parse """
module Program

let a = 10
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Binding", "UselessBinding")>]
let a = a"""

        Assert.IsFalse(this.ErrorExistsOnLine(6))

    [<Test>]
    member this.UslessBindingWithParens() = 
        this.Parse """
module Program

let a = 10
let ((a)) = ((a))"""

        Assert.IsTrue(this.ErrorExistsAt(5, 4))
        
    [<Test>]
    member this.WildcardNamedWithAsPattern() = 
        this.Parse """
module Program

match [] with
    | _ as x -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(5, 6))
        
    [<Test>]
    member this.WildcardNamedWithAsPatternSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Binding", "WildcardNamedWithAsPattern")>]
let f =
    match [] with
        | _ as x -> ()"""

        Assert.IsFalse(this.ErrorExistsOnLine(7))

    [<Test>]
    member this.NamedPattern() = 
        this.Parse """
module Program

match [] with
    | x -> ()"""

        Assert.IsFalse(this.ErrorExistsAt(5, 6))

    [<Test>]
    member this.TupleOfWildcards() = 
        this.Parse """
module Program

type Cat = | Persian of int * int

match Persian(1, 3) with
    | Persian(_, _) -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(7, 14))