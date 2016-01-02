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

module TestAst

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
    { 
        SynAttribute.AppliesToGetterAndSetter = false
        SynAttribute.Range = range()
        SynAttribute.Target = None
        SynAttribute.TypeName = LongIdentWithDots.LongIdentWithDots([Ident(name, range())], [])
        SynAttribute.ArgExpr = argExpr
    }

let stubPropertyInitialiser propertyName value =
    SynExpr.App(ExprAtomicFlag.NonAtomic, 
                false, 
                SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.Ident(Ident("op_Equality", range())), SynExpr.Ident(Ident(propertyName, range())), range()), 
                stubConstString value, 
                range())

let stubCurrentNodeInfo suppressedMessages =
    {
        Node = Type(SynType.Anon(range()))
        ChildNodes = []
        Breadcrumbs = []
        SuppressedMessages = List.map (fun x -> (x, range())) suppressedMessages
    }

[<TestFixture>]
type TestAst() =

    [<Test>]
    member __.GetSuppressMessageAttributesWithConstructorArgs() = 
        let attributes = 
            [
                [stubConstString "Analyser"; stubConstString "Rule"] 
                    |> stubTuple 
                    |> stubParen 
                    |> stubAttribute "SuppressMessage"
            ]

        let binding = AstNode.Binding(stubBinding attributes)

        let attrs = getSuppressMessageAttributes binding

        Assert.AreEqual({ Category = "Analyser"; Rule = "Rule" }, attrs |> List.head |> fst)

    [<Test>]
    member __.GetSuppressMessageAttributesWithPropertyInitialisers() = 
        let attributes = 
            [
                [stubPropertyInitialiser "Category" "Analyser"; stubPropertyInitialiser "CheckId" "*"] 
                    |> stubTuple 
                    |> stubParen 
                    |> stubAttribute "SuppressMessage"
            ]

        let binding = AstNode.Binding(stubBinding attributes)

        let attrs = getSuppressMessageAttributes binding

        Assert.AreEqual({ Category = "Analyser"; Rule = "*" }, attrs |> List.head |> fst)

    [<Test>]
    member __.GetSuppressMessageAttributesWithPropertyInitialisersMissingCategoryProperty() = 
        let attributes = 
            [
                [stubPropertyInitialiser "SomeProp" "Analyser"; stubPropertyInitialiser "CheckId" "*"] 
                    |> stubTuple 
                    |> stubParen 
                    |> stubAttribute "SuppressMessage"
            ]

        let binding = AstNode.Binding(stubBinding attributes)

        Assert.IsEmpty(getSuppressMessageAttributes binding)

    [<Test>]
    member __.GetSuppressMessageAttributesWithPropertyInitialisersMissingCheckIdProperty() = 
        let attributes = 
            [
                [stubPropertyInitialiser "Category" "Analyser"; stubPropertyInitialiser "SomeProp" "*"] 
                    |> stubTuple 
                    |> stubParen 
                    |> stubAttribute "SuppressMessage"
            ]

        let binding = AstNode.Binding(stubBinding attributes)

        Assert.IsEmpty(getSuppressMessageAttributes binding)

    [<Test>]
    member __.GetSuppressMessageAttributesWithPropertyInitialisersWithExtraProperty() = 
        let attributes = 
            [
                [
                    stubPropertyInitialiser "AnotherProp" "gwegweg"
                    stubPropertyInitialiser "Category" "Analyser"
                    stubPropertyInitialiser "CheckId" "*"
                ] 
                    |> stubTuple 
                    |> stubParen 
                    |> stubAttribute "SuppressMessage"
            ]

        let binding = AstNode.Binding(stubBinding attributes)

        let attrs = getSuppressMessageAttributes binding

        Assert.AreEqual({ Category = "Analyser"; Rule = "*" }, attrs |> List.head |> fst)

    [<Test>]
    member __.IsAnalyserSuppressedWithAllAnalyserRulesSuppressed() = 
        let currentNodeInfo = stubCurrentNodeInfo [{ Category = "Analyser"; Rule = "*" }]

        Assert.IsTrue(currentNodeInfo.IsSuppressed("Analyser"))

    [<Test>]
    member __.IsAnalyserWithDifferentNameSuppressedWithAllAnalyserRulesSuppressed() = 
        let currentNodeInfo = stubCurrentNodeInfo [{ Category = "SomeAnalyser"; Rule = "*" }]

        Assert.IsFalse(currentNodeInfo.IsSuppressed("Analyser"))

    [<Test>]
    member __.IsAnalyserSuppressedWithSpecificAnalyserRuleSuppressed() = 
        let currentNodeInfo = stubCurrentNodeInfo [{ Category = "Analyser"; Rule = "Rule" }]

        Assert.IsFalse(currentNodeInfo.IsSuppressed("Analyser"))

    [<Test>]
    member __.IsRuleSuppressedWithSpecificAnalyserRuleSuppressed() = 
        let currentNodeInfo = stubCurrentNodeInfo [{ Category = "Analyser"; Rule = "Rule" }]

        Assert.IsTrue(currentNodeInfo.IsSuppressed("Analyser", "Rule"))

    [<Test>]
    member __.IsRuleSuppressedWithAllAnalyserRulesSuppressed() = 
        let currentNodeInfo = stubCurrentNodeInfo [{ Category = "Analyser"; Rule = "*" }]

        Assert.IsTrue(currentNodeInfo.IsSuppressed("Analyser", "Rule"))

    [<Test>]
    member __.GetSuppressMessageAttributesFromAst() =
        let input =
            """
[<SuppressMessage("Analyser", "Rule")>]
module Foo =
    [<SuppressMessage("Analyser", "Rule")>]
    let dog =
        let Cat = ()
        Cat
        
    [<SuppressMessage("Analyser", "Rule")>]
    type Dog = { Woof: int }

[<SuppressMessage("Analyser", "Rule")>]
let dog = ()"""

        let stubConfig =
            {
                UseTypeChecker = Some(false)
                IgnoreFiles =
                    Some({ Update = IgnoreFiles.Overwrite
                           Files = []
                           Content = "" })
                Analysers = Map.ofList []
            }

        match parseSource input stubConfig (FSharpChecker.Create()) with
            | ParseFileResult.Success(result) ->
                Assert.AreEqual(4, getSuppressMessageAttributesFromAst result.Ast |> List.length)
            | _ -> failwith "Failed to parse input."

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/125
    [<Test>]
    member __.``Deep AST should not cause a stack overflow.``() =
        let input = @"let x = [" + (String.concat "" [for _ in 0..5000 -> "0;"]) + "]"

        let stubConfig =
            { UseTypeChecker = Some(false)
              IgnoreFiles =
                { IgnoreFiles.IgnoreFilesConfig.Update = IgnoreFiles.Overwrite
                  IgnoreFiles.IgnoreFilesConfig.Files = []
                  IgnoreFiles.IgnoreFilesConfig.Content = "" } |> Some
              Analysers = Map.ofList [] }

        match parseSource input stubConfig (FSharpChecker.Create()) with
        | ParseFileResult.Success(result) -> 
            let visitWholeTree = fun _ _ -> 
                Assert.Less(System.Diagnostics.StackTrace().FrameCount, 1000)
                Continue

            lintFile (fun _ -> false) result [visitWholeTree]
        | _ -> failwith "Failed to parse input."

    [<Test>]
    member __.GetStringLiteralsFromAst() =
        let input =
            """
module Foo =
    let value = "value"

let dog = "dog"
let escaped = @"test" """

        let stubConfig =
            {
                UseTypeChecker = Some(false)
                IgnoreFiles =
                    Some({ Update = IgnoreFiles.Overwrite
                           Files = []
                           Content = "" })
                Analysers = Map.ofList []
            }

        match parseSource input stubConfig (FSharpChecker.Create()) with
            | ParseFileResult.Success(result) ->
                Assert.AreEqual(3, getStringLiteralsFromAst result.Ast |> List.length)
            | _ -> failwith "Failed to parse input."
