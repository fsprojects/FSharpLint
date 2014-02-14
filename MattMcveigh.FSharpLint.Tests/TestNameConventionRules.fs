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

        Assert.IsFalse(errorRanges.Any(fun r -> r.StartColumn = 3 && r.EndColumn = 8))

    [<Test>]
    member self.ClassNameIsCamelCase() = 
        parse """
module Program
  type myClass2() as self =
    member this.PrintMessage() = ()"""

        Assert.IsTrue(errorRanges.Any(fun r -> r.StartLine = 3 && r.StartColumn = 7))

    [<Test>]
    member self.ClassMemberIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.ClassMemberIsCamelCase() = 
        Assert.Fail()

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
        Assert.Fail()

    [<Test>]
    member self.UnionCaseIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.RecordNameIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.RecordNameIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.RecordFieldIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.RecordFieldIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.TypeAbbreviationIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.TypeAbbreviationIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.ModuleNameIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.ModuleNameIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.NamespaceIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.NamespaceIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.VariablePatternMatchIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.VariablePatternMatchIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.TupleIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.TupleIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.PatternMatchAsIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.PatternMatchAsIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.DefaultMemberIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.DefaultMemberIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.DelegateNameIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.DelegateNameIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.FunctionNameIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.FunctionNameIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.FunctionParameterIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.FunctionParameterIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.ConstructorParameterIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.ConstructorParameterIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.StructNameIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.StructNameIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.PropertyIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.PropertyIsCamelCase() = 
        Assert.Fail()

    [<Test>]
    member self.AbstractMemberNameIsPascalCase() = 
        Assert.Fail()

    [<Test>]
    member self.AbstractMemberNameIsCamelCase() = 
        Assert.Fail()