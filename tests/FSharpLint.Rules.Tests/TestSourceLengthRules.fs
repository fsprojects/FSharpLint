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
open FSharpLint.Framework.LoadVisitors

[<Literal>]
let FunctionLength = 70

[<Literal>]
let LambdaFunctionLength = 5

[<Literal>]
let MatchLambdaFunctionLength = 70

[<Literal>]
let ValueLength = 70

[<Literal>]
let ConstructorLength = 70

[<Literal>]
let MemberLength = 70

[<Literal>]
let PropertyLength = 70

[<Literal>]
let ClassLength = 500

[<Literal>]
let UnionLength = 500

[<Literal>]
let RecordLength = 500

[<Literal>]
let EnumLength = 500

[<Literal>]
let ModuleLength = 1000

let config = 
    Map.ofList 
        [ 
            (AnalyserName, 
                { 
                    Rules = Map.ofList 
                        [ 
                            ("MaxLinesInFunction", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(FunctionLength)) 
                                        ] 
                                }) 
                            ("MaxLinesInLambdaFunction", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(LambdaFunctionLength)) 
                                        ] 
                                }) 
                            ("MaxLinesInMatchLambdaFunction", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(MatchLambdaFunctionLength)) 
                                        ] 
                                }) 
                            ("MaxLinesInValue", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(ValueLength)) 
                                        ] 
                                }) 
                            ("MaxLinesInConstructor", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(ConstructorLength)) 
                                        ] 
                                }) 
                            ("MaxLinesInMember", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(MemberLength)) 
                                        ] 
                                }) 
                            ("MaxLinesInProperty", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(PropertyLength))
                                        ] 
                                }) 
                            ("MaxLinesInClass", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(ClassLength)) 
                                        ] 
                                }) 
                            ("MaxLinesInEnum", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(EnumLength)) 
                                        ] 
                                }) 
                            ("MaxLinesInUnion", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(UnionLength)) 
                                        ] 
                                }) 
                            ("MaxLinesInRecord", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(RecordLength)) 
                                        ] 
                                }) 
                            ("MaxLinesInModule", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                            ("Lines", Lines(ModuleLength)) 
                                        ] 
                                }) 
                        ] 
                    Settings = Map.ofList []
                }) 
        ]

let generateNewLines numNewLines =
    Array.create numNewLines "\n" |> String.concat ""

[<TestFixture>]
type TestSourceLengthRules() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config)

    [<Test>]
    member this.ModuleTooManyLines() = 
        this.Parse (sprintf """
module Program
%s
// Some exception.
exception SomeException of string""" (generateNewLines ModuleLength))

        Assert.IsTrue(this.ErrorExistsAt(2, 0))

    [<Test>]
    member this.ModuleTooManyLinesSuppressed() = 
        this.Parse (sprintf """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInModule")>]
module Program
%s
// Some exception.
exception SomeException of string""" (generateNewLines ModuleLength))

        Assert.IsFalse(this.ErrorExistsOnLine(3))

    [<Test>]
    member this.ModuleNotTooManyLines() = 
        this.Parse (sprintf """
module Program
%s
// Some exception.
exception SomeException of string""" (generateNewLines (ModuleLength - 4)))

        Assert.IsFalse(this.ErrorExistsAt(2, 0))

    [<Test>]
    member this.FunctionTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog x =
    %s
    ()""" (generateNewLines FunctionLength))

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.FunctionTooManyLinesSuppressed() = 
        this.Parse (sprintf """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInFunction")>]
let dog x =
    %s
    ()""" (generateNewLines FunctionLength))

        Assert.IsFalse(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.FunctionNotTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog x =
    %s
    ()""" (generateNewLines (FunctionLength - 4)))

        Assert.IsFalse(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.ValueTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog =
    %s
    ()""" (generateNewLines ValueLength))

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.ValueTooManyLinesSuppressed() = 
        this.Parse (sprintf """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInValue")>]
let dog =
    %s
    ()""" (generateNewLines ValueLength))

        Assert.IsFalse(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.ValueNotTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog =
    %s
    ()""" (generateNewLines (ValueLength - 4)))

        Assert.IsFalse(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.MatchFunctionTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog = function
| Some(x) ->
    %s
    ()
| None -> ()""" (generateNewLines MatchLambdaFunctionLength))

        Assert.IsTrue(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.MatchFunctionTooManyLinesSuppressed() = 
        this.Parse (sprintf """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInMatchLambdaFunction")>]
let dog = function
| Some(x) ->
    %s
    ()
| None -> ()""" (generateNewLines MatchLambdaFunctionLength))

        Assert.IsFalse(this.ErrorExistsAt(5, 10))

    [<Test>]
    member this.MatchFunctionNotTooManyLines() = 
        this.Parse (sprintf """
module Program

let dog = function
| Some(x) ->
    %s
    ()
| None -> ()""" (generateNewLines (MatchLambdaFunctionLength - 5)))

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
        """ (generateNewLines LambdaFunctionLength))

        Assert.IsTrue(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.LambdaFunctionTooManyLinesSuppressed() = 
        this.Parse (sprintf """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInLambdaFunction")>]
let dog = fun x ->
    match x with
        | Some(x) ->
            %s
            ()
        | None -> ()
        """ (generateNewLines LambdaFunctionLength))

        Assert.IsFalse(this.ErrorExistsOnLine(5))

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
    member this.ClassTooManyLinesSuppressed() = 
        this.Parse (sprintf """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInClass")>]
  type MyClass2() as this =
    %s
    member this.PrintMessage() = ()""" (generateNewLines 500))

        Assert.IsFalse(this.ErrorExistsOnLine(4))

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
    abstract member Print : unit -> unit""" (generateNewLines ClassLength))

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.InterfaceTooManyLinesSuppressed() = 
        this.Parse (sprintf """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInClass")>]
  type IPrintable =
    %s
    abstract member Print : unit -> unit""" (generateNewLines ClassLength))

        Assert.IsFalse(this.ErrorExistsOnLine(4))

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
      """ (generateNewLines ConstructorLength))

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.ConstructorTooManyLinesSuppressed() = 
        this.Parse (sprintf """
module Program
[<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInConstructor")>]
type MyClass(x) =
    new() = 
      %s
      MyClass(0)
      """ (generateNewLines ConstructorLength))

        Assert.IsFalse(this.ErrorExistsOnLine(5))

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
    }""" (generateNewLines RecordLength))


        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.RecordTooManyLinesSuppressed() = 
        this.Parse (sprintf """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInRecord")>]
  type Record = 
    {
      %s 
      dog: int 
    }""" (generateNewLines RecordLength))


        Assert.IsFalse(this.ErrorExistsOnLine(4))

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
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInProperty")>]
  type Class() =
    let mutable value = 10
    member this.Property1 with get() = 
        %s
        value""" (generateNewLines PropertyLength))

        Assert.IsFalse(this.ErrorExistsOnLine(6))

    [<Test>]
    member this.PropertyTooManyLinesSuppressed() = 
        this.Parse (sprintf """
module Program
  type Class() =
    let mutable value = 10
    member this.Property1 with get() = 
        %s
        value""" (generateNewLines PropertyLength))

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