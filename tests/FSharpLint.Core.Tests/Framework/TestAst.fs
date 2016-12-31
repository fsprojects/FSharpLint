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

module TestAst

open System
open NUnit.Framework
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.ParseFile
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

let stubBinding attributes =
    SynBinding.Binding(None, 
                       SynBindingKind.NormalBinding, 
                       false, 
                       false, 
                       attributes, 
                       PreXmlDoc.PreXmlDocEmpty, 
                       SynValData(None, SynValInfo.SynValInfo([], SynArgInfo.SynArgInfo([], false, None)), None), 
                       SynPat.Wild(range()), 
                       None, 
                       SynExpr.Null(range()), 
                       range(), 
                       SequencePointInfoForBinding.NoSequencePointAtLetBinding)

let stubConstString str = SynExpr.Const(SynConst.String(str, range()), range())

let stubTuple exprs = SynExpr.Tuple(exprs, [], range())

let stubParen expr = SynExpr.Paren(expr, range(), None, range())

let stubAttribute name argExpr =
    { SynAttribute.AppliesToGetterAndSetter = false
      SynAttribute.Range = range()
      SynAttribute.Target = None
      SynAttribute.TypeName = LongIdentWithDots.LongIdentWithDots([Ident(name, range())], [])
      SynAttribute.ArgExpr = argExpr }

let stubPropertyInitialiser propertyName value =
    SynExpr.App(ExprAtomicFlag.NonAtomic, 
                false, 
                SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.Ident(Ident("op_Equality", range())), SynExpr.Ident(Ident(propertyName, range())), range()), 
                stubConstString value, 
                range())

[<TestFixture>]
type TestAst() =

    [<Test>]
    member __.GetSuppressMessageAttributesWithConstructorArgs() = 
        let attributes = 
            [ [stubConstString "Analyser"; stubConstString "Rule"] 
              |> stubTuple 
              |> stubParen 
              |> stubAttribute "SuppressMessage" ]

        let binding = AstNode.Binding(stubBinding attributes)

        let attrs = getSuppressMessageAttributes binding

        Assert.AreEqual({ Category = "Analyser"; Rule = "Rule" }, attrs |> List.head |> fst)

    [<Test>]
    member __.GetSuppressMessageAttributesWithPropertyInitialisers() = 
        let attributes = 
            [ [stubPropertyInitialiser "Category" "Analyser"; stubPropertyInitialiser "CheckId" "*"] 
              |> stubTuple 
              |> stubParen 
              |> stubAttribute "SuppressMessage" ]

        let binding = AstNode.Binding(stubBinding attributes)

        let attrs = getSuppressMessageAttributes binding

        Assert.AreEqual({ Category = "Analyser"; Rule = "*" }, attrs |> List.head |> fst)

    [<Test>]
    member __.GetSuppressMessageAttributesWithPropertyInitialisersMissingCategoryProperty() = 
        let attributes = 
            [ [stubPropertyInitialiser "SomeProp" "Analyser"; stubPropertyInitialiser "CheckId" "*"] 
              |> stubTuple 
              |> stubParen 
              |> stubAttribute "SuppressMessage" ]

        let binding = AstNode.Binding(stubBinding attributes)

        Assert.IsEmpty(getSuppressMessageAttributes binding)

    [<Test>]
    member __.GetSuppressMessageAttributesWithPropertyInitialisersMissingCheckIdProperty() = 
        let attributes = 
            [ [stubPropertyInitialiser "Category" "Analyser"; stubPropertyInitialiser "SomeProp" "*"] 
              |> stubTuple 
              |> stubParen 
              |> stubAttribute "SuppressMessage" ]

        let binding = AstNode.Binding(stubBinding attributes)

        Assert.IsEmpty(getSuppressMessageAttributes binding)

    [<Test>]
    member __.GetSuppressMessageAttributesWithPropertyInitialisersWithExtraProperty() = 
        let attributes = 
            [ [ stubPropertyInitialiser "AnotherProp" "gwegweg"
                stubPropertyInitialiser "Category" "Analyser"
                stubPropertyInitialiser "CheckId" "*" ] 
              |> stubTuple 
              |> stubParen 
              |> stubAttribute "SuppressMessage" ]

        let binding = AstNode.Binding(stubBinding attributes)

        let attrs = getSuppressMessageAttributes binding

        Assert.AreEqual({ Category = "Analyser"; Rule = "*" }, attrs |> List.head |> fst)

    [<Test>]
    member __.``TryFindTextOfRange gets expected text from given ranges``() = 
        let visitorInfo =
            { VisitorInfo.Config = { UseTypeChecker = None; IgnoreFiles = None; Analysers = Map.empty }
              VisitorInfo.FSharpVersion = Version()
              VisitorInfo.Suggest = ignore
              VisitorInfo.Text = "123\n345\n678" }

        let textOfRange (line1, col1) (line2, col2) = 
            visitorInfo.TryFindTextOfRange(mkRange "" (mkPos line1 col1) (mkPos line2 col2))
            
        Assert.AreEqual(Some "123", textOfRange (1, 0) (1, 3))
        Assert.AreEqual(Some "345", textOfRange (2, 0) (2, 3))
        Assert.AreEqual(Some "678", textOfRange (3, 0) (3, 3))

        Assert.AreEqual(Some "1", textOfRange (1, 0) (1, 1))
        Assert.AreEqual(Some "8", textOfRange (3, 2) (3, 3))

        Assert.AreEqual(Some "123\n345\n678", textOfRange (1, 0) (3, 3))