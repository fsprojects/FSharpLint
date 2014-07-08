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
open FSharpLint.Framework.LoadVisitors

let config = 
    Map.ofList 
        [ 
            (AnalyserName, 
                { 
                    Rules = Map.ofList 
                        [ 
                            ("MaxNumberOfFunctionParameters", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("MaxItems", MaxItems(5)) 
                                        ] 
                                }) 
                            ("MaxNumberOfItemsInTuple", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("MaxItems", MaxItems(5)) 
                                        ] 
                                }) 
                            ("MaxNumberOfMembers", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("MaxItems", MaxItems(5)) 
                                        ] 
                                }) 
                            ("MaxNumberOfBooleanOperatorsInCondition", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("MaxItems", MaxItems(4)) 
                                        ] 
                                }) 
                        ]
                    Settings = Map.ofList []
                }) 
        ]

[<TestFixture>]
type TestNumberOfItemsRules() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config)

    [<Test>]
    member this.SixParameters() = 
        this.Parse """
module Program

let foo one two three four five six = ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 32))

    [<Test>]
    member this.FiveParameters() = 
        this.Parse """
module Program

let foo one two three four five = ()"""

        Assert.IsFalse(this.ErrorExistsAt(4, 27))

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
    member this.FiveClassProperties() = 
        this.Parse """
module Program

type Test() =
    member val One = 0 with get, set
    member val Two = 0 with get, set
    member val Three = 0 with get, set
    member val Four = 0 with get, set
    member val Five = 0 with get, set"""

        Assert.IsFalse(this.ErrorExistsAt(9, 11))

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

        Assert.IsFalse(this.ErrorExistsAt(9, 4))

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

        Assert.IsFalse(this.ErrorExistsAt(10, 19))

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

        Assert.IsFalse(this.ErrorExistsAt(9, 11))

    [<Test>]
    member this.SixTupleItemsExpressionConstructor() = 
        this.Parse """
module Program

type Test(a,b,c,d,e,f) =
    member this.One() = ()

let dog = Test(1,2,3,4,5,6)"""

        Assert.IsFalse(this.ErrorExistsAt(7, 25))

    [<Test>]
    member this.SixTupleItemsExpressionConstructorWithNew() = 
        this.Parse """
module Program

type Test(a,b,c,d,e,f) =
    member this.One() = ()

let dog = new Test(1,2,3,4,5,6)"""

        Assert.IsFalse(this.ErrorExistsAt(7, 29))

    [<Test>]
    member this.SixTupleItemsExpressionCallingMethod() = 
        this.Parse """
module Program

type Test() =
    member this.One(a,b,c,d,e,f) = ()

let test = Test()

let dog =
    test.One(1,2,3,4,5,6)"""

        Assert.IsFalse(this.ErrorExistsAt(10, 23))

    [<Test>]
    member this.SixTupleItemsExpression() = 
        this.Parse """
module Program

let foo = (1, 2, 3, 4, 5, 6)"""

        Assert.IsTrue(this.ErrorExistsAt(4, 26))

    [<Test>]
    member this.FiveTupleItemsExpression() = 
        this.Parse """
module Program

let foo = (1, 2, 3, 4, 5)"""

        Assert.IsFalse(this.ErrorExistsAt(4, 23))

    [<Test>]
    member this.FourBooleanOperators() = 
        this.Parse """
module Program

if not true && (false && false) || true then
    ()"""

        Assert.IsFalse(this.ErrorExistsAt(4, 3))

    [<Test>]
    member this.FiveBooleanOperators() = 
        this.Parse """
module Program

if not true && (false && false) || true (&&) (false) then
    ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 3))