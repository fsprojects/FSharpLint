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

let config = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.ofList 
                [ ("FavourIgnoreOverLetWild", ruleEnabled) 
                  ("UselessBinding", ruleEnabled) 
                  ("WildcardNamedWithAsPattern", ruleEnabled) 
                  ("TupleOfWildcards", ruleEnabled) ]
              Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]
              
[<TestFixture>]
type TestBindingRules() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

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

        Assert.IsFalse(this.ErrorsExist)

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

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.UslessBinding() = 
        this.Parse("""
module Program

let a = 10
let a = a""", checkInput = true)

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.NotUslessBindingAsShadowingMutableWithImmutable() = 
        this.Parse """
module Program

let mutable a = 10
let a = a"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.NotUslessBindingAsShadowingImmutableWithMutable() = 
        this.Parse """
module Program

let a = 10
let mutable a = a"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.UslessBindingSuppressed() = 
        this.Parse """
module Program

let a = 10
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Binding", "UselessBinding")>]
let a = a"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.UslessBindingWithParens() = 
        this.Parse("""
module Program

let a = 10
let ((a)) = ((a))""", checkInput = true)

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/101
    /// (a use binding will dispose the value so is not useless)
    [<Test>]
    member this.UseBindingWithSameNameDoesNotCauseUselessBindingError() = 
        this.Parse("""
module Program

type Cat() =
    static member CreateList(reader:TextReader) = 
        use reader = reader
        reader.ReadToEnd()""", checkInput = true)
        
        Assert.IsFalse(this.ErrorsExist)
        
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
        
        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.NamedPattern() = 
        this.Parse """
module Program

match [] with
    | x -> ()"""
    
        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.TupleOfWildcards() = 
        this.Parse """
module Program

type Cat = | Persian of int * int

match Persian(1, 3) with
    | Persian(_, _) -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(7, 14))

    [<Test>]
    member this.``Method's parameter list of wildcards should not be treated as tuple of wildcards.``() = 
        this.Parse """
module Program

type Cat() = 
    member __.Persian(_, _) = ()"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Constructor's parameter list of wildcards should not be treated as tuple of wildcards.``() = 
        this.Parse """
module Program

type Cat() = 
    new(_, _) = Cat()

    member __.Persian(_) = ()"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Method with type argument's parameter list of wildcards should not be treated as tuple of wildcards.``() = 
        this.Parse """
module Program

type Cat() = 
    member __.Persian<'t>(_, _) = ()"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Method's parameter list of wildcards in object expressions should not be treated as tuple of wildcards.``() =
        this.Parse """
module Program

type I =
    abstract member Two : bool * bool -> bool

let x =
    { new I with
        member __.Two(_, _) = false }"""

        Assert.IsFalse(this.ErrorsExist)