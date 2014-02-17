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

module TestNameConventionRules

open NUnit.Framework
open System.Linq
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open MattMcveigh.FSharpLint.NameConventionRules
open MattMcveigh.FSharpLint.Ast

[<TestFixture>]
type TestNameConventionRules() =
    let stubFilename = "/home/user/Dog.test.fsx"

    let parse = parse stubFilename

    let errorRanges = System.Collections.Generic.List<range>()

    let postError (range:range) error =
        errorRanges.Add(range)

    let parse input = parse input [namingConventionVisitor postError]

    [<SetUp>]
    member self.SetUp() = 
        errorRanges.Clear()

    [<Test>]
    member self.IsPascalCase() = 
        Assert.IsTrue(isPascalCase <| Ident("DogInBin", range()))

        Assert.IsFalse(isPascalCase <| Ident("dogInBin", range()))

    [<Test>]
    member self.IsCamelCase() = 
        Assert.IsTrue(isCamelCase <| Ident("dogInBin", range()))

        Assert.IsFalse(isCamelCase <| Ident("DogInBin", range()))

    [<Test>]
    member self.ContainsUnderScore() = 
        Assert.IsTrue(containsUnderscore <| Ident("dog_", range()))

        Assert.IsFalse(containsUnderscore <| Ident("dog", range()))

    [<Test>]
    member self.ClassNameIsPascalCase() = 
        parse """
module Program
  type MyClass2() as self =
    member this.PrintMessage() = ()"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartColumn = 3 && r.StartColumn = 7))

    [<Test>]
    member self.ClassNameIsCamelCase() = 
        parse """
module Program
  type myClass2() as self =
    member this.PrintMessage() = ()"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.InterfaceNameBeginsWithI() = 
        parse """
module Program
  type IPrintable =
    abstract member Print : unit -> unit"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.InterfaceNameDoesNotBeginWithI() = 
        parse """
module Program
  type Printable =
    abstract member Print : unit -> unit"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.ClassMemberIsPascalCase() = 
        parse """
module Program
  type MyClass2() as self =
    member this.PrintMessage() = ()"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 4 && r.StartColumn = 16))

    [<Test>]
    member self.ClassMemberIsCamelCase() = 
        parse """
module Program
  type MyClass2() as self =
    member this.printMessage() = ()"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 4 && r.StartColumn = 16))

    [<Test>]
    member self.EnumNameIsPascalCase() = 
        parse """
module Program
  type MyEnum =
    | EnumCase = 1"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.EnumNameIsCamelCase() = 
        parse """
module Program
  type myEnum =
    | EnumCase = 1"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.EnumCaseIsPascalCase() = 
        parse """
module Program
  type MyEnum =
    | EnumCase = 1"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 4 && r.StartColumn = 6))

    [<Test>]
    member self.EnumCaseIsCamelCase() = 
        parse """
module Program
  type MyEnum =
    | enumCase = 1"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 4 && r.StartColumn = 6))

    [<Test>]
    member self.UnionNameIsPascalCase() = 
        parse """
module Program
  type Union =
    | Some"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.UnionNameIsCamelCase() = 
        parse """
module Program
  type union =
    | Some"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.UnionCaseIsPascalCase() = 
        parse """
module Program
  type Union =
    | UnionCase = 1"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 4 && r.StartColumn = 6))

    [<Test>]
    member self.UnionCaseIsCamelCase() = 
        parse """
module Program
  type Union =
    | unionCase = 1"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 4 && r.StartColumn = 6))

    [<Test>]
    member self.RecordNameIsPascalCase() = 
        parse """
module Program
  type Record = { dog: int }"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.RecordNameIsCamelCase() = 
        parse """
module Program
  type record = { dog: int }"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.RecordFieldIsPascalCase() = 
        parse """
module Program
  type record = { Dog: int }"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 18))

    [<Test>]
    member self.RecordFieldIsCamelCase() = 
        parse """
module Program
  type record = { dog: int }"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 18))

    [<Test>]
    member self.TypeAbbreviationIsPascalCase() = 
        parse """
module Program
  type Cat = int"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.TypeAbbreviationIsCamelCase() = 
        parse """
module Program
  type cat = int"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.ModuleNameIsPascalCase() = 
        parse """
module Program
  let main = ()"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 2 && r.StartColumn = 7))

    [<Test>]
    member self.ModuleNameIsCamelCase() = 
        parse """
module program
  let main = ()"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 2 && r.StartColumn = 7))

    [<Test>]
    member self.NamespaceIsPascalCase() = 
        parse """
namespace Matt.Dog.Cat"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 2 && r.StartColumn = 10))

    [<Test>]
    member self.NamespaceIsCamelCase() = 
        parse """
namespace matt.dog.cat"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 2 && r.StartColumn = 10))

    [<Test>]
    member self.VariablePatternMatchIsPascalCase() = 
        parse """
module program
  let main = 
    match true with
    | Dog -> ()"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 5 && r.StartColumn = 6))

    [<Test>]
    member self.VariablePatternMatchIsCamelCase() = 
        parse """
module program
  let main = 
    match true with
    | dog -> ()"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 5 && r.StartColumn = 6))

    [<Test>]
    member self.TupleIsPascalCase() = 
        parse """
module program
  let main = 
    let (Cat, _) = 1, 0"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 4 && r.StartColumn = 9))

    [<Test>]
    member self.TupleIsCamelCase() = 
        parse """
module program
  let main = 
    let (cat, _) = 1, 0"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 4 && r.StartColumn = 9))

    [<Test>]
    member self.PatternMatchAsIsPascalCase() = 
        parse """
module program
  let main = 
    match true with
    | _ as Dog -> ()"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 5 && r.StartColumn = 11))

    [<Test>]
    member self.PatternMatchAsIsCamelCase() = 
        parse """
module program
  let main = 
    match true with
    | _ as dog -> ()"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 5 && r.StartColumn = 11))

    [<Test>]
    member self.DelegateNameIsPascalCase() = 
        parse """
module program
  type Delegate2 = delegate of int * int -> int"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.DelegateNameIsCamelCase() = 
        parse """
module program
  type delegate2 = delegate of int * int -> int"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.FunctionNameIsPascalCase() = 
        parse """
module program
  let Main () = ()"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 6))

    [<Test>]
    member self.FunctionNameIsCamelCase() = 
        parse """
module program
  let main () = ()"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 6))

    [<Test>]
    member self.FunctionParameterIsPascalCase() = 
        parse """
module program
  let main Dog = ()"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 11))

    [<Test>]
    member self.FunctionParameterIsCamelCase() = 
        parse """
module program
  let main dog = ()"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 11))

    [<Test>]
    member self.ConstructorParameterIsPascalCase() = 
        parse """
module Program
  type MyClass2(Cats) as self =
    member this.PrintMessage() = ()"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartColumn = 3 && r.EndColumn = 16))

    [<Test>]
    member self.ConstructorParameterIsCamelCase() = 
        parse """
module Program
  type MyClass2(cats) as self =
    member this.PrintMessage() = ()"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartColumn = 3 && r.EndColumn = 16))

    [<Test>]
    member self.StructNameIsPascalCase() = 
        parse """
module program
  type Point2D =
    struct 
      val X: float
      val Y: float
      new(x: float, y: float) = { X = x; Y = y }
    end"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.StructNameIsCamelCase() = 
        parse """
module program
  type point2D =
    struct 
      val X: float
      val Y: float
      new(x: float, y: float) = { X = x; Y = y }
    end"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.PropertyIsPascalCase() = 
        parse """
  type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.CenterX with get() = x and set xval = x <- xval"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 6 && r.StartColumn = 16))

    [<Test>]
    member self.PropertyIsCamelCase() = 
        parse """
  type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.centerX with get() = x and set xval = x <- xval"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 6 && r.StartColumn = 16))

    [<Test>]
    member self.AbstractMemberNameIsPascalCase() = 
        parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member Rotate: float -> unit"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 5 && r.StartColumn = 20))

    [<Test>]
    member self.AbstractMemberNameIsCamelCase() = 
        parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member rotate: float -> unit"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 5 && r.StartColumn = 20))

    [<Test>]
    member self.AbstractPropertyNameIsPascalCase() = 
        parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract Area : float with get"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 5 && r.StartColumn = 13))

    [<Test>]
    member self.AbstractPropertyNameIsCamelCase() = 
        parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract area : float with get"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 5 && r.StartColumn = 13))

    [<Test>]
    member self.DefaultMemberIsPascalCase() = 
        parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member Rotate: float -> unit
    default this.Rotate(angle) = ()"""

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartLine = 6 && r.StartColumn = 17))

    [<Test>]
    member self.DefaultMemberIsCamelCase() = 
        parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member rotate: float -> unit
    default this.rotate(angle) = ()"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 6 && r.StartColumn = 17))