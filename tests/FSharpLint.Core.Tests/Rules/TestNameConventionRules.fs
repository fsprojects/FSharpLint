// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

module TestNameConventionRules

open NUnit.Framework
open FSharpLint.Rules.NameConventions
open FSharpLint.Framework.Configuration

let config =
    let pascalRule underscores = 
        { Rule.Settings = Map.ofList [
                            ("Enabled", Enabled(true))
                            ("Naming", Naming(Naming.PascalCase))
                            ("Underscores", Underscores(underscores)) ] }

    let camelRule underscores = 
        { Rule.Settings = Map.ofList [
                                ("Enabled", Enabled(true))
                                ("Naming", Naming(Naming.CamelCase))
                                ("Underscores", Underscores(underscores)) ] }

    let interfaceRule = { Rule.Settings = Map.ofList [
                                            ("Enabled", Enabled(true))
                                            ("Naming", Naming(Naming.PascalCase))
                                            ("Underscores", Underscores(NamingUnderscores.None))
                                            ("Prefix", Prefix("I")) ] }

    let exceptionRule = { Rule.Settings = Map.ofList [
                                            ("Enabled", Enabled(true))
                                            ("Naming", Naming(Naming.PascalCase))
                                            ("Underscores", Underscores(NamingUnderscores.None))
                                            ("Suffix", Suffix("Exception")) ] }

    let publicOrMeasureRule underscores = 
        { Rule.Settings = Map.ofList [
                                ("Enabled", Enabled(true))
                                ("Underscores", Underscores(underscores)) ] }

    Map.ofList
        [ (AnalyserName,
            { Rules = Map.ofList
                [ ("InterfaceNames", interfaceRule)
                  ("ExceptionNames", exceptionRule)
                  ("TypeNames", pascalRule NamingUnderscores.None)
                  ("RecordFieldNames", pascalRule NamingUnderscores.None)
                  ("EnumCasesNames", pascalRule NamingUnderscores.None)
                  ("UnionCasesNames", pascalRule NamingUnderscores.None)
                  ("ModuleNames", pascalRule NamingUnderscores.None)
                  ("LiteralNames", pascalRule NamingUnderscores.None)
                  ("NamespaceNames", pascalRule NamingUnderscores.None)
                  ("MemberNames", pascalRule NamingUnderscores.AllowPrefix)
                  ("ParameterNames", camelRule NamingUnderscores.AllowPrefix)
                  ("MeasureTypeNames", publicOrMeasureRule NamingUnderscores.None)
                  ("ActivePatternNames", pascalRule NamingUnderscores.None)
                  ("PublicValuesNames", publicOrMeasureRule NamingUnderscores.AllowPrefix)
                  ("NonPublicValuesNames", camelRule NamingUnderscores.AllowPrefix) ]
              Settings = Map.ofList [] }) ]

[<TestFixture>]
type TestNameConventionRules() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

    [<Category("Performance")>]
    [<Test>]
    member this.``Performance of naming analyser``() =
        Assert.Less(this.TimeAnalyser(100, defaultConfiguration), 20)

    [<Test>]
    member __.IsPascalCase() =
        Assert.IsTrue(isPascalCase "DogInBin")

        Assert.IsFalse(isPascalCase "dogInBin")

    [<Test>]
    member __.IsCamelCase() =
        Assert.IsTrue(isCamelCase "dogInBin")

        Assert.IsFalse(isCamelCase "DogInBin")

    [<Test>]
    member __.ContainsUnderScore() =
        Assert.IsTrue(containsUnderscore "dog_")

        Assert.IsTrue(containsUnderscore "_dog")

        Assert.IsTrue(containsUnderscore "d_og")

        Assert.IsFalse(containsUnderscore "dog")

    [<Test>]
    member this.``Unit of measure issues no casing naming warning.``() =
        this.Parse """
[<Measure>] type L

[<Measure>] type usGal"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``Unit of measure issues underscore naming warning.``() =
        this.Parse """
[<Measure>] type us_Gal"""

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.ClassNameIsPascalCase() =
        this.Parse """
module Program
  type MyClass2() as this =
    member this.PrintMessage() = ()"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.ClassNameIsCamelCase() =
        this.Parse """
module Program
  type myClass2() as this =
    member this.PrintMessage() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.ClassNameIsCamelCaseSuppressed() =
        this.Parse """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "TypeNames")>]
  type myClass2() as this =
    member this.PrintMessage() = ()"""
    
        this.AssertNoWarnings()

    [<Test>]
    member this.InterfaceNameBeginsWithI() =
        this.Parse """
module Program
  type IPrintable =
    abstract member Print : unit -> unit"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/100
    /// (static classes were thought to be interfaces)
    [<Test>]
    member this.StaticClassIsNotTreatedAsInterface() =
        this.Parse """
module Program
  type Printable =
    static member Print() = ()"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    /// Regression test for https://github.com/ionide/ionide-vscode-fsharp/issues/153
    /// (type aliases were thought to be interfaces)
    [<Test>]
    member this.TypeAliasIsNotTreatedAsInterface() =
        this.Parse """
module Program
  type Matrix = int[,]"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.InterfaceNameDoesNotBeginWithI() =
        this.Parse """
module Program
  type Printable =
    abstract member Print : unit -> unit"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.AbstractClassNameDoesNotBeginWithI() =
        this.Parse """
module program
  [<AbstractClass>]
  type Printable() =
    abstract member Print : unit -> unit"""

        Assert.IsFalse(this.ErrorExistsAt(6, 7))

    [<Test>]
    member this.InterfaceNameDoesNotBeginWithISuppressed() =
        this.Parse """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "InterfaceNames")>]
  type Printable =
    abstract member Print : unit -> unit"""
    
        this.AssertNoWarnings()

    [<Test>]
    member this.ClassMemberIsPascalCase() =
        this.Parse """
module Program
  type MyClass2() as this =
    member this.PrintMessage() = ()"""

        Assert.IsFalse(this.ErrorExistsAt(4, 16))

    [<Test>]
    member this.ClassMemberIsCamelCase() =
        this.Parse """
module Program
  type MyClass2() as this =
    member this.printMessage() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 16))

    /// The new member (constructor) is not pascal case so check it does not post an error.
    [<Test>]
    member this.ConstructorDoesNotPostError() =
        this.Parse """
module Program
type MyClass(x) =
    new() = MyClass(0)"""

        Assert.IsFalse(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.EnumNameIsPascalCase() =
        this.Parse """
module Program
  type MyEnum =
    | EnumCase = 1"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.EnumNameIsCamelCase() =
        this.Parse """
module Program
  type myEnum =
    | EnumCase = 1"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.EnumCaseIsPascalCase() =
        this.Parse """
module Program
  type MyEnum =
    | EnumCase = 1"""

        Assert.IsFalse(this.ErrorExistsAt(4, 6))

    [<Test>]
    member this.EnumCaseIsCamelCase() =
        this.Parse """
module Program
  type MyEnum =
    | enumCase = 1"""

        Assert.IsTrue(this.ErrorExistsAt(4, 6))

    [<Test>]
    member this.EnumCaseIsCamelCaseSuppressed() =
        this.Parse """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "EnumCasesNames")>]
  type MyEnum =
    | enumCase = 1"""

        this.AssertNoWarnings()

    [<Test>]
    member this.UnionNameIsPascalCase() =
        this.Parse """
module Program
  type Union =
    | Some"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.UnionNameIsCamelCase() =
        this.Parse """
module Program
  type union =
    | Some"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.UnionCaseIsPascalCase() =
        this.Parse """
module Program
  type Union =
    | UnionCase = 1"""

        Assert.IsFalse(this.ErrorExistsAt(4, 6))

    [<Test>]
    member this.UnionCaseIsCamelCase() =
        this.Parse """
module Program
  type Union =
    | unionCase = 1"""

        Assert.IsTrue(this.ErrorExistsAt(4, 6))

    [<Test>]
    member this.RecordNameIsPascalCase() =
        this.Parse """
module Program
  type Record = { dog: int }"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.RecordNameIsCamelCase() =
        this.Parse """
module Program
  type record = { dog: int }"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.RecordFieldIsPascalCase() =
        this.Parse """
module Program
  type Record = { Dog: int }"""

        Assert.IsFalse(this.ErrorExistsAt(3, 18))

    [<Test>]
    member this.RecordFieldIsCamelCase() =
        this.Parse """
module Program
  type Record = { dog: int }"""

        Assert.IsTrue(this.ErrorExistsAt(3, 18))

    [<Test>]
    member this.RecordFieldIsCamelCaseSuppressed() =
        this.Parse """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "RecordFieldNames")>]
  type Record = { dog: int }"""

        this.AssertNoWarnings()

    [<Test>]
    member this.TypeAbbreviationIsPascalCase() =
        this.Parse """
module Program
  type Cat = int"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.TypeAbbreviationIsCamelCase() =
        this.Parse """
module Program
  type cat = int"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.ModuleNameIsPascalCase() =
        this.Parse """
module Program
  let main = ()"""

        Assert.IsFalse(this.ErrorExistsAt(2, 7))

    [<Test>]
    member this.ModuleNameIsCamelCase() =
        this.Parse """
module program
  let main = ()"""

        Assert.IsTrue(this.ErrorExistsAt(2, 7))

    [<Test>]
    member this.ModuleNameIsCamelCaseSuppressed() =
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "ModuleNames")>]
module program
  let main = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.NamespaceIsPascalCase() =
        this.Parse """
namespace Matt.Dog.Cat"""

        Assert.IsFalse(this.ErrorExistsAt(2, 10))

    [<Test>]
    member this.NamespaceIsCamelCase() =
        this.Parse """
namespace matt.dog.cat"""

        Assert.IsTrue(this.ErrorExistsAt(2, 10))

    [<Test>]
    member this.LiteralPatternMatchExpectNoErrors() =
        this.Parse """
module program
  [<Literal>]
  let Dog = true

  let main =
    match true with
    | Dog -> ()
    | _ -> ()"""

        Assert.IsFalse(this.ErrorExistsAt(8, 6))

    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/103
    [<Test>]
    member this.MnemonicWildcardInPatternMatch() =
        this.Parse """
module Program
  let main =
    match true with
    | _dog -> ()
    | _ -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.UnderscoreInMatchPatternIdent() =
        this.Parse """
module program
  let main =
    match true with
    | d_og -> ()
    | _ -> ()"""

        Assert.IsTrue(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.VariablePatternMatchIsCamelCase() =
        this.Parse """
module program
  let main =
    match true with
    | dog -> ()"""

        Assert.IsFalse(this.ErrorExistsAt(5, 6))

    /// A public binding let binding identifier may be pascal case or upper case.
    [<Test>]
    member this.PublicTupleIsPascalCase() =
        this.Parse """
module program

    let (Cat, _) = 1, 0"""

        Assert.IsFalse(this.ErrorExistsAt(4, 9))

    /// A tuple inside a binding should be treated as private.
    [<Test>]
    member this.TupleInsideBindingExprIsPascalCase() =
        this.Parse """
module program

  let main =
    let (Cat, _) = 1, 0"""

        Assert.IsTrue(this.ErrorExistsAt(5, 9))

    [<Test>]
    member this.PrivateTupleIsPascalCase() =
        this.Parse """
module program
  let private Cat, private dog = 1, 0"""

        Assert.IsTrue(this.ErrorExistsAt(3, 14))

    [<Test>]
    member this.PrivateTupleIsPascalCaseSuppressed() =
        this.Parse """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "NonPublicValuesNames")>]
  let private Cat, private dog = 1, 0"""
  
        this.AssertNoWarnings()

    [<Test>]
    member this.PublicTupleIsCamelCase() =
        this.Parse """
module program
  let main =
    let (cat, _) = 1, 0"""

        Assert.IsFalse(this.ErrorExistsAt(4, 9))

    [<Test>]
    member this.PatternMatchAsIsPascalCase() =
        this.Parse """
module program
  let main =
    match true with
    | _ as Dog -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(5, 11))

    [<Test>]
    member this.PatternMatchAsIsCamelCase() =
        this.Parse """
module program
  let main =
    match true with
    | _ as dog -> ()"""

        Assert.IsFalse(this.ErrorExistsAt(5, 11))

    [<Test>]
    member this.DelegateNameIsPascalCase() =
        this.Parse """
module program
  type Delegate2 = delegate of int * int -> int"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.DelegateNameIsCamelCase() =
        this.Parse """
module program
  type delegate2 = delegate of int * int -> int"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    /// A public binding let binding identifier may be pascal case or upper case.
    [<Test>]
    member this.PublicFunctionNameIsPascalCase() =
        this.Parse """
module program
  let Main () = ()"""

        Assert.IsFalse(this.ErrorExistsAt(3, 6))

    [<Test>]
    member this.PrivateFunctionNameIsPascalCase() =
        this.Parse """
module program
  let private Main () = ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 14))

    [<Test>]
    member this.FunctionNameNestedInBindingIsPascalCase() =
        this.Parse """
module program
  let main () =
    let Main () = ()
    ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.FunctionNameNestedInBindingIsCamelCase() =
        this.Parse """
module program
  let main () =
    let bain () = ()
    ()"""

        Assert.IsFalse(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.PublicFunctionNameIsCamelCase() =
        this.Parse """
module Program
  let main () = ()"""
  
        this.AssertNoWarnings()

    [<Test>]
    member this.FunctionParameterIsPascalCase() =
        this.Parse """
module program
  let main Dog = ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 11))

    [<Test>]
    member this.FunctionParameterIsPascalCaseSuppressed() =
        this.Parse """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "NonPublicValuesNames")>]
  let main Dog = ()"""
  
        this.AssertNoWarnings()

    [<Test>]
    member this.FunctionParameterIsCamelCase() =
        this.Parse """
module program
  let main dog = ()"""

        Assert.IsFalse(this.ErrorExistsAt(3, 11))

    [<Test>]
    member this.ConstructorParameterIsPascalCase() =
        this.Parse """
module Program
  type MyClass2(Cats) as this =
    member this.PrintMessage() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 16))

    [<Test>]
    member this.ConstructorParameterIsCamelCase() =
        this.Parse """
module Program
  type MyClass2(cats) as this =
    member this.PrintMessage() = ()"""

        Assert.IsFalse(this.ErrorExistsAt(3, 16))

    [<Test>]
    member this.StructNameIsPascalCase() =
        this.Parse """
module program
  type Point2D =
    struct
      val X: float
      val Y: float
      new(x: float, y: float) = { X = x; Y = y }
    end"""

        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.StructNameIsCamelCase() =
        this.Parse """
module program
  type point2D =
    struct
      val X: float
      val Y: float
      new(x: float, y: float) = { X = x; Y = y }
    end"""

        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.PropertyIsPascalCase() =
        this.Parse """
  type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.CenterX with get() = x and set xval = x <- xval"""

        Assert.IsFalse(this.ErrorExistsAt(6, 16))

    [<Test>]
    member this.PropertyIsCamelCase() =
        this.Parse """
  type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.centerX with get() = x and set xval = x <- xval"""

        Assert.IsTrue(this.ErrorExistsAt(6, 16))

    [<Test>]
    member this.PropertyIsCamelCaseSuppressed() =
        this.Parse """
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "MemberNames")>]
  type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.centerX with get() = x and set xval = x <- xval"""
    
        this.AssertNoWarnings()

    [<Test>]
    member this.AbstractMemberNameIsPascalCase() =
        this.Parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member Rotate: float -> unit"""

        Assert.IsFalse(this.ErrorExistsAt(5, 20))

    [<Test>]
    member this.AbstractMemberNameIsCamelCase() =
        this.Parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member rotate: float -> unit"""

        Assert.IsTrue(this.ErrorExistsAt(5, 20))

    [<Test>]
    member this.AbstractPropertyNameIsPascalCase() =
        this.Parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract Area : float with get"""

        Assert.IsFalse(this.ErrorExistsAt(5, 13))

    [<Test>]
    member this.AbstractPropertyNameIsCamelCase() =
        this.Parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract area : float with get"""

        Assert.IsTrue(this.ErrorExistsAt(5, 13))

    [<Test>]
    member this.DefaultMemberIsPascalCase() =
        this.Parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member Rotate: float -> unit
    default this.Rotate(angle) = ()"""

        Assert.IsFalse(this.ErrorExistsAt(6, 17))

    [<Test>]
    member this.DefaultMemberIsCamelCase() =
        this.Parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member rotate: float -> unit
    default this.rotate(angle) = ()"""

        Assert.IsTrue(this.ErrorExistsAt(6, 17))

    [<Test>]
    member this.PatternFunctionValidActivePattern() =
        this.Parse """
module Program
let (|Even|Odd|) = function
| i when i % 2 = 0 -> Even
| _ -> Odd

match 4 with
| Even -> ()
| Odd -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PatternFunctionValidPartialActivePattern() =
        this.Parse """
module Program
let (|Even|_|) = function
| i when i % 2 = 0 -> Some(i)
| _ -> None"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ActivePatternContainsUnderscore() =
        this.Parse """
module program
let (|Ev_en|Odd|) input = if input % 2 = 0 then Ev_en else Odd

match 4 with
| Ev_en -> ()
| Odd -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 5))

    [<Test>]
    member this.ActivePatternContainsUnderscoreSuppressed() =
        this.Parse """
module Program
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "ActivePatternNames")>]
let (|Ev_en|Odd|) input = if input % 2 = 0 then Ev_en else Odd

match 4 with
| Ev_en -> ()
| Odd -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ActivePatternDoesNotContainUnderscore() =
        this.Parse """
module Program
let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

match 4 with
| Even -> ()
| Odd -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PartialActivePatternContainsUnderscore() =
        this.Parse """
module program
let (|Ev_en|_|) input = if input % 2 = 0 then Some 4 else None

match 3 with
| Ev_en(x) -> ()
| dog -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 5))

    [<Test>]
    member this.PartialActivePatternDoesNotContainUnderscore() =
        this.Parse """
module Program
let (|Even|_|) input = if input % 2 = 0 then Some 5 else None

match 3 with
| Even(x) -> ()
| dog -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ExceptionIsPascalCase() =
        this.Parse """
module program
exception MyError of string
"""

        let error = "Consider changing `MyError` to PascalCase."

        Assert.IsFalse(this.ErrorWithMessageExistsAt(error, 3, 10))

    [<Test>]
    member this.ExceptionIsCamelCase() =
        this.Parse """
module program
exception myError of string
"""

        let error = "Consider changing `myError` to PascalCase."

        Assert.IsTrue(this.ErrorWithMessageExistsAt(error, 3, 10))

    [<Test>]
    member this.ExceptionEndsWithException() =
        this.Parse """
module program
exception MyErrorException of string
"""

        let error = "Consider changing `MyErrorException` to be suffixed with 'Exception'."

        Assert.IsFalse(this.ErrorWithMessageExistsAt(error, 3, 10))

    [<Test>]
    member this.ExceptionDoesNotEndWithException() =
        this.Parse """
module Program
exception MyError of string
"""

        let error = "Consider changing `MyError` to be suffixed with `Exception`."

        Assert.IsTrue(this.ErrorWithMessageExistsAt(error, 3, 10))

    [<Test>]
    member this.ExceptionDoesNotEndWithExceptionSuppressed() =
        this.Parse """
module Program
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "ExceptionNames")>]
exception MyError of string
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ForLoopIdentifierIsCamelCase() =
        this.Parse """
module program
for i = 10 downto 1 do System.Console.Write(i)
"""

        Assert.IsFalse(this.ErrorExistsAt(3, 4))

    [<Test>]
    member this.ForLoopIdentifierIsPascalCase() =
        this.Parse """
module program
for I = 10 downto 1 do System.Console.Write(I)
"""

        Assert.IsTrue(this.ErrorExistsAt(3, 4))

    [<Test>]
    member this.ForEachLoopIdentifierIsCamelCase() =
        this.Parse """
module program
for i in 1..10 do System.Console.Write(i)
"""

        Assert.IsFalse(this.ErrorExistsAt(3, 4))

    [<Test>]
    member this.ForEachLoopIdentifierIsPascalCase() =
        this.Parse """
module program
for I in 1..10 do System.Console.Write(I)
"""

        Assert.IsTrue(this.ErrorExistsAt(3, 4))

    [<Test>]
    member this.CompilerGeneratedArgumentName() =
        this.Parse """
module program
(fun _ -> ())
"""

        Assert.IsFalse(this.ErrorExistsAt(3, 5))

    [<Test>]
    member this.TypeExtensionMethodIsPascalCase() =
        this.Parse """
module program

type MyClass() =
    member this.F() = 100

type MyClass with
    member this.Goat() = 200"""

        Assert.IsFalse(this.ErrorExistsAt(8, 16))

    [<Test>]
    member this.TypeExtensionMethodIsCamelCase() =
        this.Parse """
module program

type MyClass() =
    member this.F() = 100

type MyClass with
    member this.goat() = 200"""

        Assert.IsTrue(this.ErrorExistsAt(8, 16))

    [<Test>]
    member this.TypeExtensionTypeIsCamelCase() =
        this.Parse """
module program

type myClass() =
    member this.F() = 100

type myClass with
    member this.Goat() = 200"""

        Assert.IsFalse(this.ErrorExistsAt(7, 5))

    [<Test>]
    member this.TypeExtensionTypeIsPascalCase() =
        this.Parse """
module program

type MyClass() =
    member this.F() = 100

type MyClass with
    member this.Goat() = 200"""

        Assert.IsFalse(this.ErrorExistsAt(7, 5))

    [<Test>]
    member this.LiteralIsPascalCase() =
        this.Parse """
module program

[<Literal>]
let Cat = 5"""

        Assert.IsFalse(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.LiteralIsCamelCase() =
        this.Parse """
module program

[<Literal>]
let cat = 5"""

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.LiteralIsCamelCaseWithParen() =
        this.Parse """
module program

[<Literal>]
let (cat) = 5"""

        Assert.IsTrue(this.ErrorExistsAt(5, 5))

    [<Test>]
    member this.LiteralIsCamelCaseSuppressed() =
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "LiteralNames")>]
[<Literal>]
let cat = 5"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FullyQualifiedLiteralIsPascalCase() =
        this.Parse """
module program

[<Microsoft.FSharp.Core.Literal>]
let Cat = 5"""

        Assert.IsFalse(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.FullyQualifiedLiteralIsCamelCase() =
        this.Parse """
module program

[<Microsoft.FSharp.Core.Literal>]
let cat = 5"""

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.CamelCaseLetBindingInType() =
        this.Parse """
module program

type Dog() =
    let cat() = ()

    member this.Goat() = ()"""

        Assert.IsFalse(this.ErrorExistsAt(5, 8))

    [<Test>]
    member this.PascalCaseLetBindingInType() =
        this.Parse """
module program

type Dog() =
    let Cat() = ()

    member this.Goat() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(5, 8))

    [<Test>]
    member this.PascalCaseLetBindingInMethod() =
        this.Parse """
module program

type Cat() =
  member this.ContainsBinding() =
    let Goat = 0
    ()"""

        Assert.IsTrue(this.ErrorExistsAt(6, 8))

    [<Test>]
    member this.CamelCaseLetBindingInMethod() =
        this.Parse """
module program

type Cat() =
  member this.ContainsBinding() =
    let goat = 0
    ()"""

        Assert.IsFalse(this.ErrorExistsAt(6, 8))

    [<Test>]
    member this.PascalCaseTypeAbbreviationOfLiteral() =
        this.Parse ("""
module program

type Abbreviation = LiteralAttribute

[<Abbreviation>]
let Dog = 6""", checkInput = true)

        Assert.IsFalse(this.ErrorExistsAt(7, 4))

    [<Test>]
    member this.CamelCaseTypeAbbreviationOfLiteral() =
        this.Parse("""
module program

type Abbreviation = LiteralAttribute

[<Abbreviation>]
let dog = 6""", checkInput = true)

        Assert.IsTrue(this.ErrorExistsAt(7, 4))

    [<Test>]
    member this.ParameterUnionCaseContainingValueDoesNotGenerateWarning() =
        this.Parse("""
module Program

type SingleCaseDU = SingleCaseDU of int
let extractInt (SingleCaseDU myInt) =
  myInt

let singleCaseDU = SingleCaseDU 5

let result = extractInt singleCaseDU""", checkInput = true)

        this.AssertNoWarnings()

    [<Test>]
    member this.ParameterUnionCaseContainingPascalCaseValueGeneratesWarning() =
        this.Parse("""
module Program

type SingleCaseDU = SingleCaseDU of int
let extractInt (SingleCaseDU MyInt) =
  MyInt

let singleCaseDU = SingleCaseDU 5

let result = extractInt singleCaseDU""", checkInput = true)

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.UnionCaseInBindingContainingValueDoesNotGenerateWarning() =
        this.Parse("""
module Program

type SingleCaseDU = SingleCaseDU of int

let (SingleCaseDU myInt) = (SingleCaseDU 5)""", checkInput = true)

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.UnionCaseInBindingContainingPascalCaseValueGeneratesWarning() =
        this.Parse("""
module Program

type SingleCaseDU = SingleCaseDU of int

let (SingleCaseDU MyInt) = (SingleCaseDU 5)""", checkInput = true)

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.UnionCaseWithoutValueGeneratesWarningWhenNotTypeCheckingInput() =
        this.Parse("""
module Program

type SingleCaseDUNoValues = | SingleCaseDUNoValues

let foo SingleCaseDUNoValues = ()""", checkInput = false)

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.UnionCaseWithoutValueDoesNotGenerateWarningWhenTypeCheckingInput() =
        this.Parse("""
module Program

type SingleCaseDUNoValues = | SingleCaseDUNoValues

let foo SingleCaseDUNoValues = ()""", checkInput = true)

        this.AssertNoWarnings()

    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/99
    /// (duplicated warning for underscore in identifier).
    [<Test>]
    member this.MemberWithUnderscoreDoesNotHaveDuplicateWarnings() =
        this.Parse """
module Program

type Cat() =
    member x.Pri_nt() = ()"""

        let numberOfErrors = this.ErrorsAt(5, 13) |> Seq.length

        Assert.AreEqual(1, numberOfErrors)

    [<Test>]
    member this.``Let DU deconstruction must not warn about DU name``() =
        this.Parse """
module Program

type Foo = Foo of bool

let Foo(foo) = Foo(true)"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Let parameter DU deconstruction must not warn about DU name``() =
        this.Parse """
module Program

type Foo = Foo of bool

let foo (Foo(v)) = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``For pattern DU deconstruction must not warn about DU name``() =
        this.Parse """
module Program

type Foo = Foo of bool

for Foo(foo) in [Foo(true)] do ()"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Upper case international characters recognised by PascalCase rule``() =
        this.Parse """
module Program

type Ścieżka = Ścieżka of string
        """

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Lower case international characters recognised by camelCase rule``() =
        this.Parse """
module Program

let foo () =
    let żcieżka = 0
    ()
        """
        
        this.AssertNoWarnings()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/191
    [<Test>]
    member this.``Backticked let binding identifier not checked by name convention rules``() =
        this.Parse """
module Program

let foo () =
    let ``¯\_(ツ)_/¯`` = ignore
    ()
        """
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``When prefix of underscores is allowed expect no suggestions when the remaining member ident is PascalCase``() =
        this.Parse """
module Program

type Cat() =
    member x.__Print() = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``Quick fix for underscores with config of `None` when will remove prefixing underscores.``() = 
        let source = """
module Program

type _Cat = | Foo
"""
 
        let expected = """
module Program

type Cat = | Foo
"""
 
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Quick fix for underscores with config of `AllowPrefix` will only remove underscores not prefixing the identifier.``() = 
        let source = """
module Program

let __foo_bar = 0
"""
 
        let expected = """
module Program

let __foobar = 0
"""
 
        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)