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

module TestAbstractSyntaxArray

open System.IO
open System.Diagnostics
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AbstractSyntaxArray
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open NUnit.Framework

[<TestFixture>]
type TestAst() =

    [<Literal>]
    let SourceFile = "../../TypeChecker.fs"

    let generateAst source =
        let checker = FSharpChecker.Create()

        let options = 
            checker.GetProjectOptionsFromScript(SourceFile, source) 
            |> Async.RunSynchronously

        let parseResults =
            checker.ParseFileInProject(SourceFile, source, options)
            |> Async.RunSynchronously
        
        match parseResults.ParseTree with
        | Some(parseTree) -> parseTree
        | None -> failwith "Failed to parse file."

    [<Category("Performance")>]
    [<Test>]
    member __.``Check performance of walking tree for matching hints``() = 
        let tree = File.ReadAllText SourceFile |> generateAst

        let stopwatch = Stopwatch.StartNew()

        let array = astToArray tree

        stopwatch.Stop()

        Assert.Less(stopwatch.ElapsedMilliseconds, 200)
        System.Console.WriteLine(sprintf "Built array in %d milliseconds." stopwatch.ElapsedMilliseconds)
    
        let stopwatch = Stopwatch.StartNew()

        let length = Array.length array
        let listFoldHashCode = "List.fold".GetHashCode()

        let mutable foundListFoldIdent = false
        let mutable i = 0
        while i < length - 1 do
            let node = array.[i]
            if node.SyntaxNode = SyntaxNode.Identifier && node.Identifier = listFoldHashCode then
                foundListFoldIdent <- true
            i <- i + 1

        stopwatch.Stop()

        Assert.IsTrue(foundListFoldIdent)
        Assert.Less(stopwatch.ElapsedMilliseconds, 5)
        System.Console.WriteLine(sprintf "Iterated array in %d milliseconds." stopwatch.ElapsedMilliseconds)

    [<Test>]
    member __.``Syntax array constructed from AST in valid order.``() = 
        let tree = generateAst "List.map (fun x y -> id x) woofs"

        let array = astToArray tree

        let actual = array |> Array.map (fun x -> (x.SyntaxNode, x.Identifier))

        let expected =
            [ (SyntaxNode.FuncApp, 0)
              (SyntaxNode.Identifier, "List.map".GetHashCode())
              (SyntaxNode.Lambda, 0)
              (SyntaxNode.LambdaArg, 0)
              (SyntaxNode.Identifier, "x".GetHashCode())
              (SyntaxNode.LambdaArg, 0)
              (SyntaxNode.Identifier, "y".GetHashCode())
              (SyntaxNode.LambdaBody, 0)
              (SyntaxNode.FuncApp, 0)
              (SyntaxNode.Identifier, "id".GetHashCode())
              (SyntaxNode.Identifier, "x".GetHashCode())
              (SyntaxNode.Identifier, "woofs".GetHashCode()) ]

        Assert.AreEqual(expected, actual)