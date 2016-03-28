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

module TestHintMatcher

open System.Diagnostics
open System.IO
open NUnit.Framework
open FParsec
open FSharpLint.Framework.AbstractSyntaxArray
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.HintParser
open FSharpLint.Framework.HintMatcher
open FSharpLint.Framework.LoadVisitors
open Microsoft.FSharp.Compiler.SourceCodeServices

let generateHintConfig hints =
    let parseHints hints =
        let parseHint hint =
            match CharParsers.run phint hint with
            | FParsec.CharParsers.Success(hint, _, _) -> hint
            | FParsec.CharParsers.Failure(error, _, _) -> failwithf "Invalid hint %s" error

        List.map (fun x -> { Hint = x; ParsedHint = parseHint x }) hints

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.empty 
              Settings = Map.ofList [ ("Hints", Hints(parseHints hints)) ] }) ]
    
[<TestFixture>]
type TestHintMatcher() =
    inherit TestRuleBase.TestRuleBase(SyntaxArray(visitor getHintsFromConfig))

    [<Literal>]
    let SourceFile = "../../../FSharpLint.Framework.Tests/TypeChecker.fs"

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
    member __.``Check performance of fuzzy matching hints``() = 
        let tree = File.ReadAllText SourceFile |> generateAst

        let stopwatch = Stopwatch.StartNew()

        let (array, skipArray) = astToArray tree

        stopwatch.Stop()

        Assert.Less(stopwatch.ElapsedMilliseconds, 200)
        System.Console.WriteLine(sprintf "Built array in %d milliseconds." stopwatch.ElapsedMilliseconds)

        let config = 
            { UseTypeChecker = None
              IgnoreFiles = None
              Analysers = generateHintConfig [] }

        visitor 
            (fun _ -> MergeSyntaxTrees.Edges.Empty)  
            { FSharpVersion = System.Version(); Config = config; PostError = (fun _ _ -> ()) }
            None
            array
            skipArray

    [<Test>]
    member this.MatchNotEqualHint() = 
        let config = generateHintConfig ["not (a = b) ===> a <> b"]

        this.Parse("""
module Goat

let (a, b) = (1, 2)
let valid = not (a = b)""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 12))

    [<Test>]
    member this.MatchFunctionApplication() = 
        let config = generateHintConfig ["List.fold (+) 0 x ===> List.sum x"]

        this.Parse("""
module Goat

List.fold (+) 0 x""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchInfixExpression() = 
        let config = generateHintConfig ["4 + 4 ===> 8"]

        this.Parse("""
module Goat

4 + 4""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchPrefixExpression() = 
        let config = generateHintConfig ["4 + %4 ===> 8"]

        this.Parse("""
module Goat

4 + %4""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchAddressOfPrefixExpression() = 
        let config = generateHintConfig ["4 + &4 ===> 8"]

        this.Parse("""
module Goat

4 + &4""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchParenthesesInHintExpression() = 
        let config = generateHintConfig ["6 + (4 / (5)) ===> 8"]

        this.Parse("""
module Goat

6 + 4 / 5""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchParenthesesExpression() = 
        let config = generateHintConfig ["6 + (4 + (5)) ===> 8"]

        this.Parse("""
module Goat

6 + (4 + (5))""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchLambda() = 
        let config = generateHintConfig ["fun x _ y -> x + y ===> 0"]

        this.Parse("""
module Goat

let f = fun x y z -> x + z""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchWildcardLambda() = 
        let config = generateHintConfig ["fun _ -> 1 ===> id"]

        this.Parse("""
module Goat

let f = fun _ -> 1""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchMultipleWildcardLambda() = 
        let config = generateHintConfig ["fun _ _ -> 1 ===> id"]

        this.Parse("""
module Goat

let f = fun _ _ -> 1""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchMultipleWildcardAndVariableLambda() = 
        let config = generateHintConfig ["fun _ a _ b -> 1 ===> id"]

        this.Parse("""
module Goat

let f = fun _ a _ x -> 1""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchIdLambda() = 
        let config = generateHintConfig ["fun x -> x ===> id"]

        this.Parse("""
module Goat

let f = fun x -> x""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchIdLambdaSuppressed() = 
        let config = generateHintConfig ["fun x -> x ===> id"]

        this.Parse("""
module Goat

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Hints", "*")>]
let f = fun x -> x""", config)

        Assert.IsFalse(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.DontMatchIdLambda() = 
        let config = generateHintConfig ["fun x -> x ===> id"]

        this.Parse("""
module Goat

let f = fun x -> 1""", config)

        Assert.IsFalse(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.MatchFunctionApplicationWithBackwardPipe() = 
        let config = generateHintConfig ["(+) 1 x ===> x"]

        this.Parse("""
module Goat

(+) 1 <| 2 + 3""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 1))

    [<Test>]
    member this.MatchFunctionApplicationWithForwardPipe() = 
        let config = generateHintConfig ["List.fold (+) 0 x ===> List.sum x"]

        this.Parse("""
module Goat

[1;2;3] |> List.fold (+) 0""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchMultipleFunctionApplications() = 
        let config = generateHintConfig ["List.head (List.sort x) ===> List.min x"]

        this.Parse("""
module Goat

[1;2;3] |> List.sort |> List.head""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchTupleApplication() = 
        let config = generateHintConfig ["fst (x, y) ===> x"]

        this.Parse("""
module Goat

fst (1, 0) |> ignore""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchListAppendItem() = 
        let config = generateHintConfig ["x::[] ===> [x]"]

        this.Parse("""
module Goat

1::[] |> ignore""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchAppendListToList() = 
        let config = generateHintConfig ["[x]@[y] ===> [x;y]"]

        this.Parse("""
module Goat

[1]@[2] |> ignore""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchListAppendItemInPattern() = 
        let config = generateHintConfig ["x::[] ===> [x]"]

        this.Parse("""
module Goat

match [] with
| x::[] -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.MatchTupleInPattern() = 
        let config = generateHintConfig ["(_, []) ===> []"]

        this.Parse("""
module Goat

match ([], []) with
| (_, []) -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 3))

    [<Test>]
    member this.MatchIntegerConstantInPattern() = 
        let config = generateHintConfig ["0 ===> 0"]

        this.Parse("""
module Goat

match 0 with
| 0 -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.MatchListInPattern() = 
        let config = generateHintConfig ["[0; 1; 2] ===> 0"]

        this.Parse("""
module Goat

match [] with
| [0; 1; 2;] -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.MatchArrayInPattern() = 
        let config = generateHintConfig ["[|0; 1; 2|] ===> 0"]

        this.Parse("""
module Goat

match [] with
| [|0; 1; 2;|] -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.MatchEmptyArray() = 
        let config = generateHintConfig ["Array.isEmpty [||] ===> true"]

        this.Parse("""
module Goat

Array.isEmpty [||]""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchOrPattern() = 
        let config = generateHintConfig ["[] | [0] ===> []"]

        this.Parse("""
module Goat

match [] with
| [] | [0] -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))
        
    [<Test>]
    member this.MatchAndPattern() = 
        let config = generateHintConfig ["[] & [0] ===> []"]

        this.Parse("""
module Goat

match [] with
| [] & [0] -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.MatchMultipleAndPatterns() = 
        let config = generateHintConfig ["[] & [0] & [1] & [2] ===> []"]

        this.Parse("""
module Goat

match [] with
| [] & [0] & [1] & [2] -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))
        
    [<Test>]
    member this.MatchAndPatternsInsideMultipleAndPatterns() = 
        let config = generateHintConfig ["[0] & [1] ===> []"]

        this.Parse("""
module Goat

match [] with
| [] & [0] & [1] & [2] -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.MatchAndPatternsAndOrPatterns() = 
        let config = generateHintConfig ["[0] & [1] | [1] & [2] ===> []"]

        this.Parse("""
module Goat

match [] with
| [0] & [1] | [1] & [2] -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.MatchIfStatement() = 
        let config = generateHintConfig ["if x then true else false ===> x"]

        this.Parse("""
module Goat

if true then true else false""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchElseIfStatement() = 
        let config = generateHintConfig ["if x then true else if y then true else false ===> x || y"]

        this.Parse("""
module Goat

if true then true else if true then true else false""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchSingleParamStaticMethod() = 
        let config = generateHintConfig ["System.String.Copy x ===> x"]

        this.Parse("""
module Goat

System.String.Copy("dog")""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.MatchMultiParamStaticMethod() = 
        let config = generateHintConfig ["System.String.Compare(x, y) ===> x"]
        
        this.Parse("""
module Goat

System.String.Compare("dog", "cat")""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.NamedParameterShouldNotBeTreatedAsInfixOperation() = 
        let config = generateHintConfig ["x = true ===> x"]
        
        this.Parse("""
module Goat

type Bar() =
    static member SomeMethod(foo: bool) = ()

Bar.SomeMethod(foo = true)""", config)

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Named parameter in object method call should not be treated as infix operation``() = 
        let config = generateHintConfig ["x = true ===> x"]
        
        this.Parse("""
module Goat

type Bar() =
    member this.SomeMethod(foo: bool) = ()

Bar().SomeMethod(foo = true)""", config)

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.NamedParameterWithMoreThanOneParameterShouldNotBeTreatedAsInfixOperation() = 
        let config = generateHintConfig ["x = true ===> x"]
        
        this.Parse("""
module Goat

type Bar() =
    static member SomeMethod(woof: int, foo: bool) = ()

Bar.SomeMethod(woof = 5, foo = true)""", config)

        Assert.IsFalse(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/128
    [<Test>]
    member this.``Named parameters in non-atomic method call must not be treated as infix operations.``() = 
        let config = generateHintConfig ["x = false ===> not x"]
        
        this.Parse("""
module Goat

do
    let parser = UnionArgParser.Create<'T>()
    let results = 
        parser.Parse
            (inputs = args, raiseOnUsage = false, ignoreMissing = true, 
             errorHandler = ProcessExiter())""", config)

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Named parameter in object method call with more than one arg should not be treated as infix operation``() = 
        let config = generateHintConfig ["x = true ===> x"]
        
        this.Parse("""
module Goat

type Bar() =
    member this.SomeMethod(woof: int, foo: bool) = ()

Bar().SomeMethod(woof = 5, foo = true)""", config)

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.PropertyInitShouldNotBeTreatedAsInfixOperation() = 
        let config = generateHintConfig ["x = true ===> x"]
        
        this.Parse("""
module Goat

type Bar() =
    member val Foo = true with get, set

Bar(Foo = true) |> ignore""", config)

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.PropertyInitWithNewKeywwordShouldNotBeTreatedAsInfixOperation() = 
        let config = generateHintConfig ["x = true ===> x"]
        
        this.Parse("""
module Goat

type Bar() =
    member val Foo = true with get, set

new Bar(Foo = true) |> ignore""", config)

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.MultiplePropertyInitWithNewKeywwordShouldNotBeTreatedAsInfixOperation() = 
        let config = generateHintConfig ["x = true ===> x"]
        
        this.Parse("""
module Goat

type Bar() =
    member val Foo = true with get, set
    member val Bar = true with get, set

new Bar(Foo = true, Bar = true) |> ignore""", config)

        Assert.IsFalse(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/108
    /// Type arguments on a constructor were causing hint to be displayed for property initialisation.
    [<Test>]
    member this.PropertyInitWithTypeArgsShouldNotBeTreatedAsInfixOperation() = 
        let config = generateHintConfig ["x = true ===> x"]
        
        this.Parse("""
module Goat

type Bar<'a>() =
    member val Foo = true with get, set

Bar<_>(Foo = true) |> ignore""", config)

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.PropertyEqualityOperationShouldBeTreatedAsInfixOperation() = 
        let config = generateHintConfig ["x = true ===> x"]
        
        this.Parse("""
module Goat

type Bar() =
    member val Foo = true with get, set

    member this.X() = this.Foo = true""", config)

        Assert.IsTrue(this.ErrorExistsOnLine(7))

    /// Parentheses around expressions matched by hints were causing duplicate warnings
    [<Test>]
    member this.ParenthesesAroundAMatchedExpressionShouldNotCauseAnExtraMatch() = 
        let config = generateHintConfig ["x = true ===> x"]
        
        this.Parse("""
module Goat

let foo x = if (x = true) then 0 else 1""", config)

        Assert.IsTrue((this.ErrorExistsAt >> not)(4, 15) && this.ErrorExistsAt(4, 16))

    /// Parentheses around patterns matched by hints were causing duplicate warnings
    [<Test>]
    member this.ParenthesesAroundAMatchedPatternShouldNotCauseAnExtraMatch() = 
        let config = generateHintConfig ["[0] & [1] ===> []"]
        
        this.Parse("""
module Goat

match [] with
| ([0] & [1]) -> ()
| _ -> ()""", config)

        Assert.IsTrue((this.ErrorExistsAt >> not)(5, 2) && this.ErrorExistsAt(5, 3))

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/109
    [<Test>]
    member this.``Lambdas should not be suggested to be functions if in method call that takes delegate type.``() = 
        let config = generateHintConfig ["fun _ -> () ===> ignore"]
        
        this.Parse("""
module Goat

type TakesDelegate() =
    member this.Foo(del:System.Action<string>) = ()
    
TakesDelegate().Foo(fun _ -> ())""", config, checkInput = true)

        Assert.IsFalse(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/109
    [<Test>]
    member this.``Lambdas should not be suggested to be functions if in method call that takes delegate type (more than one argument).``() = 
        let config = generateHintConfig ["fun _ -> () ===> ignore"]
        
        this.Parse("""
module Goat

type TakesDelegate() =
    member this.Foo(foo:string, del:System.Action<string>) = ()
    
TakesDelegate().Foo("", fun _ -> ())""", config, checkInput = true)

        Assert.IsFalse(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/109
    [<Test>]
    member this.``Lambdas should be suggested to be functions if in method call that takes function type.``() = 
        let config = generateHintConfig ["fun _ -> () ===> ignore"]
        
        this.Parse("""
module Goat

type TakesDelegate() =
    member this.Foo(foo:string, del:string -> unit) = ()
    
TakesDelegate().Foo("", fun _ -> ())""", config, checkInput = true)

        Assert.IsTrue(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/109
    [<Test>]
    member this.``Lambdas should be suggested to be functions if in obj method call that takes function type (multiple args).``() = 
        let config = generateHintConfig ["fun _ -> () ===> ignore"]
        
        this.Parse("""
module Goat

type TakesDelegate() =
    member this.Foo(foo:string, del:System.Action<string>) = ()
    
let object = TakesDelegate()
object.Foo("", fun _ -> ())""", config, checkInput = true)

        Assert.IsFalse(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/109
    [<Test>]
    member this.``Lambdas should be suggested to be functions if in obj method call that takes function type.``() = 
        let config = generateHintConfig ["fun _ -> () ===> ignore"]
        
        this.Parse("""
module Goat

type TakesDelegate() =
    member this.Foo(del:System.Action<string>) = ()
    
let object = TakesDelegate()
object.Foo(fun _ -> ())""", config, checkInput = true)

        Assert.IsFalse(this.ErrorsExist)
        
    [<Test>]
    member this.``Operator identifier is correctly written out as an operator symbol in the error message.``() = 
        let config = generateHintConfig ["0 ===> FSharpLint.(+)"]
        
        this.Parse("""
module Goat

do
    ignore 0""", config, checkInput = true)

        this.ErrorWithMessageExists("`0` might be able to be refactored into `FSharpLint.( + )`.") |> Assert.IsTrue
        
    [<Test>]
    member this.``Suggestion as a message presents correct error message.``() = 
        let config = generateHintConfig ["() ===> m\"Message\""]
        
        this.Parse("""
module Goat

do
    ()""", config, checkInput = true)

        this.ErrorWithMessageExists("`()`; suggestion: Message.") |> Assert.IsTrue
        
    [<Test>]
    member this.``Hints matches null in an expression correctly.``() = 
        let config = generateHintConfig ["x = null ===> m\"Use pattern matching to null check\""]
        
        this.Parse("""
module Goat

do
    let x = System.Collections.ArrayList()
    x = null |> ignore""", config, checkInput = true)

        this.ErrorWithMessageExists("`x=null`; suggestion: Use pattern matching to null check.") |> Assert.IsTrue