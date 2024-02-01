module FSharpLint.Core.Tests.TestFuzzyHintMatcher

open System.Collections.Generic
open System.Diagnostics
open FSharpLint.Rules.Helper.Hints
open FSharpLint.Framework
open FSharpLint.Framework.AbstractSyntaxArray
open FSharpLint.Framework.HintParser
open FSharpLint.Framework.HintParser.MergeSyntaxTrees
open NUnit.Framework
open FParsec
open TestUtils

// fsharplint:disable MaxLinesInValue

let possibleMatches (syntaxArray:AbstractSyntaxArray.Node []) (hintTrie:Edges) notify =
    for i = 0 to syntaxArray.Length - 1 do
        let node = syntaxArray.[i]

        match hintTrie.Lookup.TryGetValue node.Hashcode with
        | true, trie -> checkTrie (i + 1) trie syntaxArray (Dictionary<_, _>()) (notify i)
        | false, _ -> ()

[<TestFixture>]
type TestAst() =

    let toHint hint =
        match run phint hint with
        | Success(hint, _, _) -> hint
        | Failure(message, _, _) -> failwith message

    [<Category("Performance")>]
    [<Test>]
    member _.``Performance of matching fuzzy matching hints``() =
        let (tree, _) = getPerformanceTestInput ()

        let array = astToArray tree

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
                toHint @"List.sum (List.map x y) ===> List.sumBy x y"
                toHint @"Array.sum (Array.map x y) ===> Array.sumBy x y"
                toHint @"Seq.sum (Seq.map x y) ===> Seq.sumBy x y"
                toHint @"List.average (List.map x y) ===> List.averageBy x y"
                toHint @"Array.average (Array.map x y) ===> Array.averageBy x y"
                toHint @"Seq.average (Seq.map x y) ===> Seq.averageBy x y"
                toHint @"(List.take x y, List.skip x y) ===> List.splitAt x y"
                toHint @"(Array.take x y, Array.skip x y) ===> Array.splitAt x y"
                toHint @"(Seq.take x y, Seq.skip x y) ===> Seq.splitAt x y"
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

        possibleMatches array hintTrie (fun n1 hint -> matches.Add(n1, hint))

        stopwatch.Stop()
        Assert.Less(stopwatch.ElapsedMilliseconds, 50)
        fprintf TestContext.Out "Iterated array in %d milliseconds." stopwatch.ElapsedMilliseconds

    [<Category("Hint Matcher")>]
    [<Test>]
    member _.``Lambda with wildcard argument is correctly found by fuzzy matcher``() =
        let source = @"
do
    let y = fun _ -> ()
    ()"

        let array = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"fun _ -> () ===> ignore"]

        let matches = ResizeArray()

        possibleMatches array hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member _.``Function application is correctly found by fuzzy matcher``() =
        let source = @"
do
    let y = List.isEmpty []
    ()"

        let array = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"List.isEmpty [] ===> true"]

        let matches = ResizeArray()

        possibleMatches array hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member _.``Infix application is correctly found by fuzzy matcher``() =
        let source = @"
do
    let y = 1 + 0
    ()"

        let array = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"x + 0 ===> x"]

        let matches = ResizeArray()

        possibleMatches array hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member _.``Prefix application is correctly found by fuzzy matcher``() =
        let source = @"
do
    let y = ~~~1
    ()"

        let array = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"~~~1 ===> x"]

        let matches = ResizeArray()

        possibleMatches array hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member _.``Function application with variable is correctly found by fuzzy matcher``() =
        let source = @"
do
    let numbers = [1;2;3]
    let y = numbers |> List.rev |> List.rev
    ()"

        let array = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"List.rev (List.rev x) ===> x"]

        let matches = ResizeArray()

        possibleMatches array hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member _.``Lambda with variable argument is correctly found by fuzzy matcher``() =
        let source = @"
do
    let y = fun x -> x
    ()"

        let array = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"fun x -> x ===> id"]

        let matches = ResizeArray()

        possibleMatches array hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(1, matches.Count)

    [<Category("Hint Matcher")>]
    [<Test>]
    member _.``Lambda with variable argument is correctly discarded by fuzzy matcher``() =
        let source = @"
do
    let y = fun x -> 0
    ()"

        let array = generateAst source |> astToArray

        let hintTrie = MergeSyntaxTrees.mergeHints [toHint @"fun x -> x ===> id"]

        let matches = ResizeArray()

        possibleMatches array hintTrie (fun n1 hint -> matches.Add(n1, hint))

        Assert.AreEqual(0, matches.Count)