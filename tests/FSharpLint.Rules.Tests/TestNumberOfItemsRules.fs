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

module TestNumberOfItemsRules

open NUnit.Framework
open FSharpLint.Rules.NumberOfItems
open FSharpLint.Framework.Configuration

let config = 
    Map.ofList 
        [ (AnalyserName, 
                { Rules = Map.ofList 
                    [ ("MaxNumberOfFunctionParameters", 
                        { Settings = Map.ofList 
                            [ ("Enabled", Enabled(true)) 
                              ("MaxItems", MaxItems(5)) ] }) 
                      ("MaxNumberOfItemsInTuple", 
                        { Settings = Map.ofList 
                            [ ("Enabled", Enabled(true)) 
                              ("MaxItems", MaxItems(5)) ] }) 
                      ("MaxNumberOfMembers", 
                        { Settings = Map.ofList 
                            [ ("Enabled", Enabled(true)) 
                              ("MaxItems", MaxItems(5)) ] }) 
                      ("MaxNumberOfBooleanOperatorsInCondition", 
                        { Settings = Map.ofList 
                            [ ("Enabled", Enabled(true)) 
                              ("MaxItems", MaxItems(4)) ] }) ]
                  Settings = Map.empty }) ]
                   
[<TestFixture>]
type TestNumberOfItemsRules() =
    inherit TestRuleBase.TestRuleBase(visitor, config)

    [<Test>]
    member this.SixParameters() = 
        this.Parse """
module Program

let foo one two three four five six = ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 32))

    [<Test>]
    member this.SixParametersSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("NumberOfItems", "MaxNumberOfFunctionParameters")>]
let foo one two three four five six = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FiveParameters() = 
        this.Parse """
module Program

let foo one two three four five = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.SixClassProperties() = 
        this.Parse """
module Program

type Test() =
    member val One = 0 with get, set
    member val Two = 0 with get, set
    member val Three = 0 with get, set
    member val Four = 0 with get, set
    member val Five = 0 with get, set
    member val Six = 0 with get, set"""

        Assert.IsTrue(this.ErrorExistsAt(10, 11))

    [<Test>]
    member this.SixClassPropertiesSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("NumberOfItems", "MaxNumberOfMembers")>]
type Test() =
    member val One = 0 with get, set
    member val Two = 0 with get, set
    member val Three = 0 with get, set
    member val Four = 0 with get, set
    member val Five = 0 with get, set
    member val Six = 0 with get, set"""
    
        this.AssertNoWarnings()
            
    [<Test>]
    member this.FiveClassProperties() = 
        this.Parse """
module Program

type Test() =
    member val One = 0 with get, set
    member val Two = 0 with get, set
    member val Three = 0 with get, set
    member val Four = 0 with get, set
    member val Five = 0 with get, set"""
    
        this.AssertNoWarnings()

    [<Test>]
    member this.SixClassAbstractMethods() = 
        this.Parse """
module Program

type Test() =
    abstract member One: unit -> unit
    abstract member Two: unit -> unit
    abstract member Three: unit -> unit
    abstract member Four: unit -> unit
    abstract member Five: unit -> unit
    abstract member Six: unit -> unit"""

        Assert.IsTrue(this.ErrorExistsAt(10, 4))

    [<Test>]
    member this.FiveClassAbstractMethods() = 
        this.Parse """
module Program

type Test() =
    abstract member One: unit -> unit
    abstract member Two: unit -> unit
    abstract member Three: unit -> unit
    abstract member Four: unit -> unit
    abstract member Five: unit -> unit"""
    
        this.AssertNoWarnings()

    [<Test>]
    member this.SixClassMethods() = 
        this.Parse """
module Program

type Test() =
    member this.One() = ()
    member this.Two() = ()
    member this.Three() = ()
    member this.Four() = ()
    member this.Five() = ()
    member this.Six() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(10, 11))

    [<Test>]
    member this.SixClassMethodsLastPrivate() = 
        this.Parse """
module Program

type Test() =
    member this.One() = ()
    member this.Two() = ()
    member this.Three() = ()
    member this.Four() = ()
    member this.Five() = ()
    private member this.Six() = ()"""
    
        this.AssertNoWarnings()

    [<Test>]
    member this.FiveClassMethods() = 
        this.Parse """
module Program

type Test() =
    member this.One() = ()
    member this.Two() = ()
    member this.Three() = ()
    member this.Four() = ()
    member this.Five() = ()"""
    
        this.AssertNoWarnings()

    [<Test>]
    member this.SixTupleItemsExpressionConstructor() = 
        this.Parse """
module Program

type Test(a,b,c,d,e,f) =
    member this.One() = ()

let dog = Test(1,2,3,4,5,6)"""

        this.AssertNoWarnings()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/141
    /// Note: we are just disabling all warnings for tuples in function applications
    /// because in a lot of places the user won't have control over the definition
    /// of the function - the definition of a function should be where the lint is warning.
    [<Test>]
    member this.``Tuple with too many items in a functiona application must never issue a warning.``() = 
        this.Parse """
foo (1,2,3,4,5,6)"""

        this.AssertNoWarnings()

    [<Test>]
    member this.SixTupleItemsExpressionConstructorWithNew() = 
        this.Parse """
module Program

type Test(a,b,c,d,e,f) =
    member this.One() = ()

let dog = new Test(1,2,3,4,5,6)"""

        this.AssertNoWarnings()

    [<Test>]
    member this.SixTupleItemsExpressionCallingMethod() = 
        this.Parse """
module Program

type Test() =
    member this.One(a,b,c,d,e,f) = ()

let test = Test()

let dog =
    test.One(1,2,3,4,5,6)"""
    
        this.AssertNoWarnings()

    [<Test>]
    member this.SixTupleItemsExpression() = 
        this.Parse """
module Program

let foo = (1, 2, 3, 4, 5, 6)"""

        Assert.IsTrue(this.ErrorExistsAt(4, 26))

    [<Test>]
    member this.SixTupleItemsExpressionSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("NumberOfItems", "MaxNumberOfItemsInTuple")>]
let foo = (1, 2, 3, 4, 5, 6)"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FiveTupleItemsExpression() = 
        this.Parse """
module Program

let foo = (1, 2, 3, 4, 5)"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FourBooleanOperators() = 
        this.Parse """
module Program

if not true && (false && false) || true then
    ()"""
    
        this.AssertNoWarnings()

    [<Test>]
    member this.FiveBooleanOperators() = 
        this.Parse """
module Program

if not true && (false && false) || true (&&) (false) then
    ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 3))

    [<Test>]
    member this.FiveBooleanOperatorsSuppressed() = 
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NumberOfItems", "MaxNumberOfBooleanOperatorsInCondition")>]
module Program

if not true && (false && false) || true (&&) (false) then
    ()"""
    
        this.AssertNoWarnings()