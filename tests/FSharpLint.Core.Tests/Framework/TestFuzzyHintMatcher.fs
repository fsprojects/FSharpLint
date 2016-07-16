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

module TestFuzzyHintMatcher

open System.Diagnostics
open FSharpLint.Framework.AbstractSyntaxArray
open FSharpLint.Framework.FuzzyHintMatcher
open FSharpLint.Framework.HintParser
open NUnit.Framework
open FParsec
open TestUtils

[<TestFixture>]
type TestAst() =

    let toHint hint =
        match run phint hint with
        | Success(hint, _, _) -> hint
        | Failure(message, _, _) -> failwith message

    [<Category("Performance")>]
    [<Test>]
    member __.``Performance of matching fuzzy matching hints``() = 
        let (tree, _) = getPerformanceTestInput ()

        let (array, skipArray) = astToArray tree

        let matches = ResizeArray()

        let hints =
            [   toHint @"not (a =  b) ===> a <> b"
                toHint @"not (a <> b) ===> a =  b"
                toHint @"not (a >  b) ===> a <= b"
                toHint @"not (a >= b) ===> a <  b"
                toHint @"not (a <  b) ===> a >= b"
                toHint @"not (a <= b) ===> a >  b"
                toHint @"compare x y <> 1 ===> x <= y"
                toHint @"compare x y = -1 ===> x < y"
                toHint @"compare x y <> -1 ===> x >= y"
                toHint @"compare x y = 1 ===> x > y"
                toHint @"compare x y <= 0 ===> x <= y"
                toHint @"compare x y <  0 ===> x <  y"
                toHint @"compare x y >= 0 ===> x >= y"
                toHint @"compare x y >  0 ===> x >  y"
                toHint @"compare x y =  0 ===> x =  y"
                toHint @"compare x y <> 0 ===> x <> y"
                toHint @"List.head (List.sort x) ===> List.min x"
                toHint @"List.head (List.sortBy f x) ===> List.minBy f x"
                toHint @"List.map f (List.map g x) ===> List.map (g >> f) x"
                toHint @"Array.map f (Array.map g x) ===> Array.map (g >> f) x"
                toHint @"Seq.map f (Seq.map g x) ===> Seq.map (g >> f) x"
                toHint @"List.nth x 0 ===> List.head x"
                toHint @"List.map f (List.replicate n x) ===> List.replicate n (f x)"
                toHint @"List.rev (List.rev x) ===> x"
                toHint @"Array.rev (Array.rev x) ===> x"
                toHint @"List.fold (@) [] ===> List.concat"
                toHint @"List.map id ===> id"
                toHint @"Array.map id ===> id"
                toHint @"Seq.map id ===> id"
                toHint @"(List.length x) = 0 ===> List.isEmpty x"
                toHint @"(Array.length x) = 0 ===> Array.isEmpty x"
                toHint @"(Seq.length x) = 0 ===> Seq.isEmpty x"
                toHint @"x = [] ===> List.isEmpty x"
                toHint @"x = [||] ===> Array.isEmpty x"
                toHint @"(List.length x) <> 0 ===> not (List.isEmpty x)"
                toHint @"(Array.length x) <> 0 ===> not (Array.isEmpty x)"
                toHint @"(Seq.length x) <> 0 ===> not (Seq.isEmpty x)"
                toHint @"(List.length x) > 0 ===> not (List.isEmpty x)"
                toHint @"(Array.length x) <> 0 ===> not (Array.isEmpty x)"
                toHint @"(Seq.length x) <> 0 ===> not (Seq.isEmpty x)"
                toHint @"List.isEmpty (List.filter f x) ===> not (List.exists f x)"
                toHint @"Array.isEmpty (Array.filter f x) ===> not (Array.exists f x)"
                toHint @"Seq.isEmpty (Seq.filter f x) ===> not (Seq.exists f x)"
                toHint @"not (List.isEmpty (List.filter f x)) ===> List.exists f x"
                toHint @"not (Array.isEmpty (Array.filter f x)) ===> Array.exists f x"
                toHint @"not (Seq.isEmpty (Seq.filter f x)) ===> Seq.exists f x"
                toHint @"List.length x >= 0 ===> true"
                toHint @"Array.length x >= 0 ===> true"
                toHint @"Seq.length x >= 0 ===> true"
                toHint @"x = true ===> x"
                toHint @"x = false ===> not x"
                toHint @"true = a ===> a"
                toHint @"false = a ===> not a"
                toHint @"a <> true ===> not a"
                toHint @"a <> false ===> a"
                toHint @"true <> a ===> not a"
                toHint @"false <> a ===> a"
                toHint @"if a then true else false ===> a"
                toHint @"if a then false else true ===> not a"
                toHint @"not (not x) ===> x"
                toHint @"(fst x, snd x) ===> x"
                toHint @"true && x ===> x"
                toHint @"false && x ===> false"
                toHint @"true || x ===> true"
                toHint @"false || x ===> x"
                toHint @"not true ===> false"
                toHint @"not false ===> true"
                toHint @"fst (x, y) ===> x"
                toHint @"snd (x, y) ===> y"
                toHint @"List.fold f x [] ===> x"
                toHint @"Array.fold f x [||] ===> x"
                toHint @"List.foldBack f x [] ===> x"
                toHint @"Array.foldBack f x [||] ===> x"
                toHint @"x - 0 ===> x"
                toHint @"x * 1 ===> x"
                toHint @"x / 1 ===> x"
                toHint @"List.fold (+) 0 ===> List.sum"
                toHint @"Array.fold (+) 0 ===> Array.sum"
                toHint @"Seq.fold (+) 0 ===> Seq.sum"
                toHint @"List.empty ===> []"
                toHint @"Array.empty ===> [||]"
                toHint @"x::[] ===> [x]"
                toHint @"pattern: x::[] ===> [x]"
                toHint @"x @ [] ===> x"
                toHint @"List.isEmpty [] ===> true"
                toHint @"Array.isEmpty [||] ===> true"
                toHint @"fun _ -> () ===> ignore"
                toHint @"fun x -> x ===> id"
                toHint @"id x ===> x"
                toHint @"id >> f ===> f"
                toHint @"f >> id ===> f" ]

        let hintTrie = MergeSyntaxTrees.mergeHints hints
    
        let stopwatch = Stopwatch.StartNew()
        
        possibleMatches array skipArray hintTrie (fun n1 hint -> matches.Add(n1, hint))

        stopwatch.Stop()
        Assert.Less(stopwatch.ElapsedMilliseconds, 50)
        System.Console.WriteLine(sprintf "Iterated array in %d milliseconds." stopwatch.ElapsedMilliseconds)

    [<Category("Hint Matcher")>]
    [<Test>]
    member __.``Lambda with wildcard argument is correctly found by fuzzy matcher``() = 
        let source = @"
do
    let y = fun _ -> ()
    ()"

        let (array, skipArray) = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"fun _ -> () ===> ignore"]

        let matches = ResizeArray()
        
        possibleMatches array skipArray hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member __.``Function application is correctly found by fuzzy matcher``() = 
        let source = @"
do
    let y = List.isEmpty []
    ()"

        let (array, skipArray) = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"List.isEmpty [] ===> true"]

        let matches = ResizeArray()
        
        possibleMatches array skipArray hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member __.``Infix application is correctly found by fuzzy matcher``() = 
        let source = @"
do
    let y = 1 + 0
    ()"

        let (array, skipArray) = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"x + 0 ===> x"]

        let matches = ResizeArray()
        
        possibleMatches array skipArray hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member __.``Prefix application is correctly found by fuzzy matcher``() = 
        let source = @"
do
    let y = ~~~1
    ()"

        let (array, skipArray) = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"~~~1 ===> x"]

        let matches = ResizeArray()
        
        possibleMatches array skipArray hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member __.``Function application with variable is correctly found by fuzzy matcher``() = 
        let source = @"
do
    let numbers = [1;2;3]
    let y = numbers |> List.rev |> List.rev
    ()"

        let (array, skipArray) = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"List.rev (List.rev x) ===> x"]

        let matches = ResizeArray()
        
        possibleMatches array skipArray hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)
        
    [<Category("Hint Matcher")>]
    [<Test>]
    member __.``Lambda with variable argument is correctly found by fuzzy matcher``() = 
        let source = @"
do
    let y = fun x -> x
    ()"

        let (array, skipArray) = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"fun x -> x ===> id"]

        let matches = ResizeArray()
        
        possibleMatches array skipArray hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member __.``Lambda with variable argument is correctly discarded by fuzzy matcher``() = 
        let source = @"
do
    let y = fun x -> 0
    ()"

        let (array, skipArray) = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"fun x -> x ===> id"]

        let matches = ResizeArray()
        
        possibleMatches array skipArray hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(0, matches.Count)