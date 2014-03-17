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

module TestSourceLengthRules

open NUnit.Framework
open FSharpLint.Rules.SourceLength
open FSharpLint.Framework.Configuration

let config = 
    Map.ofList 
        [ 
            ("FSharpLint.SourceLength", 
                { Rules = Map.ofList 
                    [ 
                        ("MaxLinesInFunction", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(70)) 
                                    ] 
                            }) 
                        ("MaxLinesInLambdaFunction", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(5)) 
                                    ] 
                            }) 
                        ("MaxLinesInMatchLambdaFunction", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(70)) 
                                    ] 
                            }) 
                        ("MaxLinesInValue", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(70)) 
                                    ] 
                            }) 
                        ("MaxLinesInConstructor", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(70)) 
                                    ] 
                            }) 
                        ("MaxLinesInMember", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(70)) 
                                    ] 
                            }) 
                        ("MaxLinesInProperty", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(70))
                                    ] 
                            }) 
                        ("MaxLinesInClass", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(500)) 
                                    ] 
                            }) 
                        ("MaxLinesInEnum", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(500)) 
                                    ] 
                            }) 
                        ("MaxLinesInUnion", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(500)) 
                                    ] 
                            }) 
                        ("MaxLinesInRecord", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(500)) 
                                    ] 
                            }) 
                        ("MaxLinesInModule", 
                            { 
                                Settings = Map.ofList 
                                    [ 
                                        ("Enabled", Enabled(true)) 
                                        ("Lines", Lines(1000)) 
                                    ] 
                            }) 
                    ] 
                }) 
        ]

let generateNewLines numNewLines =
    [ for i in 1..numNewLines do yield "\n" ] |> String.concat ""

[<TestFixture>]
type TestSourceLengthRules() =
    inherit TestRuleBase.TestRuleBase(visitor, config)

    [<Test>]
    member this.ModuleTooManyLines() = 
        this.Parse (sprintf """
module Program
%s
// Some exception.
exception SomeException of string""" (generateNewLines (1000)))

        Assert.IsTrue(this.ErrorExistsAt(2, 0))

    [<Test>]
    member this.ModuleNotTooManyLines() = 
        this.Parse (sprintf """
module Program
%s
// Some exception.
exception SomeException of string""" (generateNewLines (1000 - 4)))

        Assert.IsFalse(this.ErrorExistsAt(2, 0))

    [<Test>]
    member this.FunctionTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog x =
    %s
    ()""" (generateNewLines 70))

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.FunctionNotTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog x =
    %s
    ()""" (generateNewLines (70 - 4)))

        Assert.IsFalse(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.ValueTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog =
    %s
    ()""" (generateNewLines 70))

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.ValueNotTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog =
    %s
    ()""" (generateNewLines (70 - 4)))

        Assert.IsFalse(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.MatchFunctionTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog = function
| Some(x) ->
    %s
    ()
| None -> ()""" (generateNewLines 70))

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.MatchFunctionNotTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog = function
| Some(x) ->
    %s
    ()
| None -> ()""" (generateNewLines (70 - 5)))

        Assert.IsFalse(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LambdaFunctionTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog = fun x ->
    match x with
        | Some(x) ->
            %s
            ()
        | None -> ()
        """ (generateNewLines 70))

        Assert.IsTrue(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.LambdaFunctionNotTooManyLines() = 
        this.Parse """
module Program

let dog = fun x ->
    match x with
        | Some(x) ->
            ()
        | None -> ()
        """

        Assert.IsFalse(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.ClassTooManyLines() = 
        this.Parse (sprintf """
module Program
  type MyClass2() as this =
    %s
    member this.PrintMessage() = ()""" (generateNewLines 500))

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.ClassNotTooManyLines() = 
        this.Parse """
module Program
  type MyClass2() as this =
    member this.PrintMessage() = ()"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.InterfaceTooManyLines() = 
        this.Parse (sprintf """
module Program
  type IPrintable =
    %s
    abstract member Print : unit -> unit""" (generateNewLines 500))

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.InterfaceNotTooManyLines() = 
        this.Parse """
module Program
  type IPrintable =
    abstract member Print : unit -> unit"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.ConstructorTooManyLines() = 
        this.Parse (sprintf """
module Program
type MyClass(x) =
    new() = 
      %s
      MyClass(0)
      """ (generateNewLines 70))

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.ConstructorNotTooManyLines() = 
        this.Parse """
module Program
type MyClass(x) =
    new() = MyClass(0)"""

        Assert.IsFalse(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.RecordTooManyLines() = 
        this.Parse (sprintf """
module Program
  type Record = 
    {
      %s 
      dog: int 
    }""" (generateNewLines 500))


        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.RecordNotTooManyLines() = 
        this.Parse """
module Program
  type Record = { dog: int }"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.PropertyTooManyLines() = 
        this.Parse (sprintf """
module Program
  type Class() =
    let mutable value = 10
    member this.Property1 with get() = 
        %s
        value""" (generateNewLines 70))

        Assert.IsTrue(this.ErrorExistsAt(5, 31))

    [<Test>]
    member this.PropertyNotTooManyLines() = 
        this.Parse """
module Program
  type Class() =
    let mutable value = 10
    member this.Property1 with get() = 
        value"""

        Assert.IsFalse(this.ErrorExistsAt(5, 31))