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

module TestHintMatcher

open System.Diagnostics
open System.IO
open NUnit.Framework
open FParsec
open FSharpLint.Framework.AbstractSyntaxArray
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.HintParser
open FSharpLint.Rules.HintMatcher
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
    inherit TestRuleBase.TestRuleBase(analyser getHintsFromConfig)

    [<Category("Performance")>]
    [<Test>]
    member this.``Performance of hint matcher analyser``() = 
        Assert.Less(this.TimeAnalyser(100, defaultConfiguration), 50)

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
    member this.``Match address of operator with a single ampersand in expression``() =  
        let config = generateHintConfig ["4 + &4 ===> 8"]

        this.Parse("""
module Goat

4 + &4""", config)

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.``Match address of operator with two ampersands in expression``() = 
        let config = generateHintConfig ["4 + &&4 ===> 8"]

        this.Parse("""
module Goat

4 + &&4""", config)

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
        let config = generateHintConfig ["pattern: x::[] ===> [x]"]

        this.Parse("""
module Goat

match [] with
| x::[] -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.MatchTupleInPattern() = 
        let config = generateHintConfig ["pattern: (_, []) ===> []"]

        this.Parse("""
module Goat

match ([], []) with
| (_, []) -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 3))

    [<Test>]
    member this.MatchIntegerConstantInPattern() = 
        let config = generateHintConfig ["pattern: 0 ===> 0"]

        this.Parse("""
module Goat

match 0 with
| 0 -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.MatchListInPattern() = 
        let config = generateHintConfig ["pattern: [0; 1; 2] ===> 0"]

        this.Parse("""
module Goat

match [] with
| [0; 1; 2;] -> ()
| _ -> ()""", config)

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

    [<Test>]
    member this.MatchArrayInPattern() = 
        let config = generateHintConfig ["pattern: [|0; 1; 2|] ===> 0"]

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
        let config = generateHintConfig ["pattern: [] | [0] ===> []"]

        this.Parse("""
module Goat

match [] with
| [] | [0] -> ()
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
        let config = generateHintConfig ["pattern: [0] | [1] ===> []"]
        
        this.Parse("""
module Goat

match [] with
| ([0] | [1]) -> ()
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
    member this.``Lambdas should not be suggested to be functions if in obj method call that takes function type (multiple args).``() = 
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
    member this.``Lambdas should not be suggested to be functions if in obj method call that takes function type.``() = 
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

        this.ErrorWithMessageExists("`x = null`; suggestion: Use pattern matching to null check.") |> Assert.IsTrue
        
    /// Regression test for: http://codereview.stackexchange.com/questions/134296/f-function-to-concatenate-some-dsl-scripts-with-indentation#comment251110_134297
    [<Test>]
    member this.``Lambda hint correctly matches expression with parameters.``() = 
        let config = generateHintConfig ["fun x -> x ===> id"]
        
        this.Parse("""
module Goat

do
    [(1,2,3)] |> Seq.groupBy(fun (store, app, script) -> store)  |> ignore""", config)

        this.AssertNoWarnings()
        
    /// Regression test for: http://stackoverflow.com/questions/38412166/how-to-refactor-a-function-using-ignore
    [<Test>]
    member this.``Lambda hint does not ignore curried parameters.``() = 
        let config = generateHintConfig ["fun _ -> () ===> ignore"]
        
        this.Parse("""
module Goat

do
    let log = fun data medium -> ()
    ()""", config)

        this.AssertNoWarnings()  
      
    
[<TestFixture>]
type TestHintMatcherFixes() =
    inherit TestRuleBase.TestRuleBase(analyser getHintsFromConfig)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/pull/194#issuecomment-268560761  
    [<Test>]
    member this.``Lambdas in hint suggestions must be surrounded with parentheses.``() = 
        let source = """
module Program

let x = [1;2;3] |> List.map (fun x -> [x]) |> List.concat
"""
 
        let expected = """
module Program

let x = List.collect (fun x -> [x]) [1;2;3]
"""
 
        this.Parse(source, generateHintConfig ["List.concat (List.map f x) ===> List.collect f x"])
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Function application moved in suggestion which does not require parentheses must not be surrounded by them.``() = 
        let source = """
module Program

let x y = if y 0 then true else false
"""
 
        let expected = """
module Program

let x y = y 0
"""
 
        this.Parse(source, generateHintConfig ["if x then true else false ===> x"])
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Function application moved in suggestion which requires parens must be surrounded by them.``() = 
        let source = """
module Program

let x y = y 0
"""
 
        let expected = """
module Program

let x y = y (foo 0)
"""
 
        this.Parse(source, generateHintConfig ["0 ===> foo 0"])
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Function application in suggestion with surrounding parens keeps them.``() = 
        let source = """
module Program

let x y = y (foo 0)
"""
 
        let expected = """
module Program

let x y = y (foo 0 0)
"""
 
        this.Parse(source, generateHintConfig ["foo 0 ===> foo 0 0"])
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Function application as variable in suggestion with surrounding parens keeps them.``() = 
        let source = """
module Program

let x y = bar (foo 0)
"""
 
        let expected = """
module Program

let x y = id (foo 0)
"""
 
        this.Parse(source, generateHintConfig ["bar x ===> id x"])
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Function application in suggestion with surrounding parens but no longer needs removes surrounding parens.``() = 
        let source = """
module Program

let x y = bar (foo 0)
"""
 
        let expected = """
module Program

let x y = if foo 0 then 0 else 1
"""
 
        this.Parse(source, generateHintConfig ["bar x ===> if x then 0 else 1"])
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Function application in suggestion with addressof operator keeps parens surrounding application.``() = 
        let source = """
module Program

let x y = &(foo 0)
"""
 
        let expected = """
module Program

let x y = &&(foo 0)
"""
 
        this.Parse(source, generateHintConfig ["&x ===> &&x"])
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Function application in suggestion with prefix operator removed, removes parens surrounding application.``() = 
        let source = """
module Program

let x y = - -(foo 0)
"""
 
        let expected = """
module Program

let x y = foo 0
"""
 
        this.Parse(source, generateHintConfig ["- -x ===> x"])
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Infix operator in hint fix is formatted with space either side of it``() = 
        let source = """
module Program

let x y =
    y
    |> List.map (fun x -> x)
    |> List.map id
"""
 
        let expected = """
module Program

let x y =
    List.map ((fun x -> x) >> id) y
"""
 
        this.Parse(source, generateHintConfig ["List.map f (List.map g x) ===> List.map (g >> f) x"])
        Assert.AreEqual(expected, this.ApplyQuickFix source)