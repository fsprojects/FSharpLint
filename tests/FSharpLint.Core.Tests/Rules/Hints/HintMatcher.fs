module rec FSharpLint.Core.Tests.Rules.Hints.HintMatcher

// fsharplint:disable TupleIndentation

open NUnit.Framework
open FSharpLint.Core.Tests

type Assert with
  static member NoViolations(this:TestHintMatcher) =
    if this.NoViolationsExist then ()
    else
        raise (NUnit.Framework.AssertionException($"No violations were expected, but there were some. Violations were:\n{this.ViolationMsg}"))

[<TestFixture; SingleThreaded>]
type TestHintMatcher() =
    inherit TestHintMatcherBase.TestHintMatcherBase()

    [<Test>]
    member this.MatchNotEqualHint() =
        this.SetConfig(["not (a = b) ===> a <> b"])

        this.Parse """
module Goat

let (a, b) = (1, 2)
let valid = not (a = b)
"""

        Assert.ViolationExistsAt(this, (5, 12))

    [<Test>]
    member this.MatchFunctionApplication() =
        this.SetConfig(["List.fold (+) 0 x ===> List.sum x"])

        this.Parse """
module Goat

List.fold (+) 0 x
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchInfixExpression() =
        this.SetConfig(["4 + 4 ===> 8"])

        this.Parse """
module Goat

4 + 4
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchPrefixExpression() =
        this.SetConfig(["4 + %4 ===> 8"])

        this.Parse """
module Goat

4 + %4
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.``Match address of operator with a single ampersand in expression``() =
        this.SetConfig(["4 + &4 ===> 8"])

        this.Parse """
module Goat

4 + &4
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.``Match address of operator with two ampersands in expression``() =
        this.SetConfig(["4 + &&4 ===> 8"])

        this.Parse """
module Goat

4 + &&4
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchParenthesesInHintExpression() =
        this.SetConfig(["6 + (4 / (5)) ===> 8"])

        this.Parse """
module Goat

6 + 4 / 5
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchParenthesesExpression() =
        this.SetConfig(["6 + (4 + (5)) ===> 8"])

        this.Parse """
module Goat

6 + (4 + (5))
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchLambda() =
        this.SetConfig(["fun x _ y -> x + y ===> 0"])

        this.Parse """
module Goat

let f = fun x y z -> x + z
"""

        Assert.ViolationExistsAt(this, (4, 8))

    [<Test>]
    member this.MatchWildcardLambda() =
        this.SetConfig(["fun _ -> 1 ===> id"])

        this.Parse """
module Goat

let f = fun _ -> 1
"""

        Assert.ViolationExistsAt(this, (4, 8))

    [<Test>]
    member this.MatchMultipleWildcardLambda() =
        this.SetConfig(["fun _ _ -> 1 ===> id"])

        this.Parse """
module Goat

let f = fun _ _ -> 1
"""

        Assert.ViolationExistsAt(this, (4, 8))

    [<Test>]
    member this.MatchMultipleWildcardAndVariableLambda() =
        this.SetConfig(["fun _ a _ b -> 1 ===> id"])

        this.Parse """
module Goat

let f = fun _ a _ x -> 1
"""

        Assert.ViolationExistsAt(this, (4, 8))

    [<Test>]
    member this.MatchIdLambda() =
        this.SetConfig(["fun x -> x ===> id"])

        this.Parse """
module Goat

let f = fun x -> x
"""

        Assert.ViolationExistsAt(this, (4, 8))

    [<Test>]
    member this.DontMatchIdLambda() =
        this.SetConfig(["fun x -> x ===> id"])

        this.Parse """
module Goat

let f = fun x -> 1
"""

        Assert.IsFalse(this.ViolationExistsAt(4, 8))

    [<Test>]
    member this.MatchFunctionApplicationWithBackwardPipe() =
        this.SetConfig(["(+) 1 x ===> x"])

        this.Parse """
module Goat

(+) 1 <| 2 + 3
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchFunctionApplicationWithForwardPipe() =
        this.SetConfig(["List.fold (+) 0 x ===> List.sum x"])

        this.Parse """
module Goat

[1;2;3] |> List.fold (+) 0
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchMultipleFunctionApplications() =
        this.SetConfig(["List.head (List.sort x) ===> List.min x"])

        this.Parse """
module Goat

[1;2;3] |> List.sort |> List.head
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchTupleApplication() =
        this.SetConfig(["fst (x, y) ===> x"])

        this.Parse """
module Goat

fst (1, 0) |> ignore
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchListAppendItem() =
        this.SetConfig(["x::[] ===> [x]"])

        this.Parse """
module Goat

1::[] |> ignore
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchAppendListToList() =
        this.SetConfig(["[x]@[y] ===> [x;y]"])

        this.Parse """
module Goat

[1]@[2] |> ignore
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchListAppendItemInPattern() =
        this.SetConfig(["pattern: x::[] ===> [x]"])

        this.Parse """
module Goat

match [] with
| x::[] -> ()
| _ -> ()
"""

        Assert.ViolationExistsAt(this, (5, 2))

    [<Test>]
    member this.MatchTupleInPattern() =
        this.SetConfig(["pattern: (_, []) ===> []"])

        this.Parse """
module Goat

match ([], []) with
| (_, []) -> ()
| _ -> ()
"""

        Assert.ViolationExistsAt(this, (5, 3))

    [<Test>]
    member this.MatchIntegerConstantInPattern() =
        this.SetConfig(["pattern: 0 ===> 0"])

        this.Parse """
module Goat

match 0 with
| 0 -> ()
| _ -> ()
"""

        Assert.ViolationExistsAt(this, (5, 2))

    [<Test>]
    member this.MatchListInPattern() =
        this.SetConfig(["pattern: [0; 1; 2] ===> 0"])

        this.Parse """
module Goat

match [] with
| [0; 1; 2;] -> ()
| _ -> ()
"""

        Assert.ViolationExistsAt(this, (5, 2))

    [<Test>]
    member this.MatchArrayInPattern() =
        this.SetConfig(["pattern: [|0; 1; 2|] ===> 0"])

        this.Parse """
module Goat

match [] with
| [|0; 1; 2;|] -> ()
| _ -> ()
"""

        Assert.ViolationExistsAt(this, (5, 2))

    [<Test>]
    member this.MatchEmptyArray() =
        this.SetConfig(["Array.isEmpty [||] ===> true"])

        this.Parse """
module Goat

Array.isEmpty [||]
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchOrPattern() =
        this.SetConfig(["pattern: [] | [0] ===> []"])

        this.Parse """
module Goat

match [] with
| [] | [0] -> ()
| _ -> ()
"""

        Assert.ViolationExistsAt(this, (5, 2))

    [<Test>]
    member this.MatchIfStatement() =
        this.SetConfig(["if x then true else false ===> x"])

        this.Parse """
module Goat

if true then true else false
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchElseIfStatement() =
        this.SetConfig(["if x then true else if y then true else false ===> x || y"])

        this.Parse """
module Goat

if true then true else if true then true else false
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchSingleParamStaticMethod() =
        this.SetConfig(["System.String.Copy x ===> x"])

        this.Parse """
module Goat

System.String.Copy("dog")
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.MatchMultiParamStaticMethod() =
        this.SetConfig(["System.String.Compare(x, y) ===> x"])

        this.Parse """
module Goat

System.String.Compare("dog", "cat")
"""

        Assert.ViolationExistsAt(this, (4, 0))

    [<Test>]
    member this.NamedParameterShouldNotBeTreatedAsInfixOperation() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Bar() =
    static member SomeMethod(foo:bool) = ()

Bar.SomeMethod(foo = true)
"""

        Assert.NoViolations(this)

    [<Test>]
    member this.``Named parameter in object method call should not be treated as infix operation``() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Bar() =
    member this.SomeMethod(foo: bool) = ()

Bar().SomeMethod(foo = true)
"""

        Assert.NoViolations(this)

    [<Test>]
    member this.NamedParameterWithMoreThanOneParameterShouldNotBeTreatedAsInfixOperation() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Bar() =
    static member SomeMethod(woof: int, foo: bool) = ()

Bar.SomeMethod(woof = 5, foo = true)
"""

        Assert.NoViolations(this)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/128
    [<Test>]
    member this.``Named parameters in non-atomic method call must not be treated as infix operations.``() =
        this.SetConfig(["x = false ===> not x"])

        this.Parse("""
module Goat

do
    let parser = UnionArgParser.Create<'T>()
    let results =
        parser.Parse
            (inputs = args, raiseOnUsage = false, ignoreMissing = true,
             errorHandler = ProcessExiter())""", checkFile=false) // This test only passes with typechecking disabled.

        Assert.NoViolations(this)

    [<Test>]
    member this.``Named parameter in object method call with more than one arg should not be treated as infix operation``() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Bar() =
    member this.SomeMethod(woof: int, foo: bool) = ()

Bar().SomeMethod(woof = 5, foo = true)
"""

        Assert.NoViolations(this)

    [<Test>]
    member this.PropertyInitShouldNotBeTreatedAsInfixOperation() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Bar() =
    member val Foo = true with get, set

Bar(Foo = true) |> ignore
"""

        Assert.NoViolations(this)

    [<Test>]
    member this.PropertyInitWithNewKeywwordShouldNotBeTreatedAsInfixOperation() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Bar() =
    member val Foo = true with get, set

new Bar(Foo = true) |> ignore
"""

        Assert.NoViolations(this)

    [<Test>]
    member this.MultiplePropertyInitWithNewKeywwordShouldNotBeTreatedAsInfixOperation() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Bar() =
    member val Foo = true with get, set
    member val Bar = true with get, set

new Bar(Foo = true, Bar = true) |> ignore
"""

        Assert.NoViolations(this)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/108
    /// Type arguments on a constructor were causing hint to be displayed for property initialisation.
    [<Test>]
    member this.PropertyInitWithTypeArgsShouldNotBeTreatedAsInfixOperation() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Bar<'a>() =
    member val Foo = true with get, set

Bar<_>(Foo = true) |> ignore
"""

        Assert.NoViolations(this)

    [<Test>]
    member this.PropertyEqualityOperationShouldBeTreatedAsInfixOperation() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Bar() =
    member val Foo = true with get, set

    member this.X() = this.Foo = true
"""

        Assert.IsTrue(this.ViolationExistsOnLine(7))

    /// Parentheses around expressions matched by hints were causing duplicate violations
    [<Test>]
    member this.ParenthesesAroundAMatchedExpressionShouldNotCauseAnExtraMatch() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

let foo x = if (x = true) then 0 else 1
"""

        Assert.IsTrue((this.ViolationExistsAt >> not)(4, 15) && this.ViolationExistsAt(4, 16))

    /// Parentheses around patterns matched by hints were causing duplicate violations
    [<Test>]
    member this.ParenthesesAroundAMatchedPatternShouldNotCauseAnExtraMatch() =
        this.SetConfig(["pattern: [0] | [1] ===> []"])

        this.Parse """
module Goat

match [] with
| ([0] | [1]) -> ()
| _ -> ()
"""

        Assert.IsTrue((this.ViolationExistsAt >> not)(5, 2) && this.ViolationExistsAt(5, 3))

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/109
    [<Test>]
    member this.``Lambdas should not be suggested to be functions if in method call that takes delegate type.``() =
        this.SetConfig(["fun _ -> () ===> ignore"])

        this.Parse """
module Goat

type TakesDelegate() =
    member this.Foo(del:System.Action<string>) = ()

TakesDelegate().Foo(fun _ -> ())
"""

        Assert.NoViolations(this)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/109
    [<Test>]
    member this.``Lambdas should not be suggested to be functions if in method call that takes delegate type (more than one argument).``() =
        this.SetConfig(["fun _ -> () ===> ignore"])

        this.Parse """
module Goat

type TakesDelegate() =
    member this.Foo(foo:string, del:System.Action<string>) = ()

TakesDelegate().Foo("", fun _ -> ())
"""

        Assert.NoViolations(this)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/109
    [<Test>]
    member this.``Lambdas should be suggested to be functions if in method call that takes function type.``() =
        this.SetConfig(["fun _ -> () ===> ignore"])

        this.Parse """
module Goat

type TakesDelegate() =
    member this.Foo(foo:string, del:string -> unit) = ()

TakesDelegate().Foo("", fun _ -> ())
"""

        Assert.IsTrue(this.ViolationsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/109
    [<Test>]
    member this.``Lambdas should not be suggested to be functions if in obj method call that takes function type (multiple args).``() =
        this.SetConfig(["fun _ -> () ===> ignore"])

        this.Parse """
module Goat

type TakesDelegate() =
    member this.Foo(foo:string, del:System.Action<string>) = ()

let object = TakesDelegate()
object.Foo("", fun _ -> ())
"""

        Assert.NoViolations(this)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/109
    [<Test>]
    member this.``Lambdas should not be suggested to be functions if in obj method call that takes function type.``() =
        this.SetConfig(["fun _ -> () ===> ignore"])

        this.Parse """
module Goat

type TakesDelegate() =
    member this.Foo(del:System.Action<string>) = ()

let object = TakesDelegate()
object.Foo(fun _ -> ())
"""

        Assert.NoViolations(this)

    [<Test>]
    member this.``Operator identifier is correctly written out as an operator symbol in the violation message.``() =
        this.SetConfig(["0 ===> FSharpLint.(+)"])

        this.Parse"""
module Goat

do
    ignore 0
"""

        this.AssertViolationWithMessageExists("`0` might be able to be refactored into `FSharpLint.( + )`.")

    [<Test>]
    member this.``Suggestion as a message presents correct violation message.``() =
        this.SetConfig(["() ===> m\"Message\""])

        this.Parse"""
module Goat

do
    ()
"""

        this.AssertViolationWithMessageExists("`()`; suggestion: Message.")

    [<Test>]
    member this.``Hints matches null in an expression correctly.``() =
        this.SetConfig(["x = null ===> m\"Use pattern matching to null check\""])

        this.Parse """
module Goat

do
    let x = System.Collections.ArrayList()
    x = null |> ignore
"""

        this.AssertViolationWithMessageExists("`x = null`; suggestion: Use pattern matching to null check.")

    /// Regression test for: http://codereview.stackexchange.com/questions/134296/f-function-to-concatenate-some-dsl-scripts-with-indentation#comment251110_134297
    [<Test>]
    member this.``Lambda hint correctly matches expression with parameters.``() =
        this.SetConfig(["fun x -> x ===> id"])

        this.Parse """
module Goat

do
    [(1,2,3)] |> Seq.groupBy(fun (store, app, script) -> store)  |> ignore
"""

        this.AssertNoViolations()

    /// Regression test for: http://stackoverflow.com/questions/38412166/how-to-refactor-a-function-using-ignore
    [<Test>]
    member this.``Lambda hint does not ignore curried parameters.``() =
        this.SetConfig(["fun _ -> () ===> ignore"])

        this.Parse """
module Goat

do
    let log = fun data medium -> ()
    ()
"""

        this.AssertNoViolations()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/304
    [<Test>]
    member this.``Equality hint must not match an expression that's assigning a field within a constructor.``() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Foo =
    new() = { X = false }
    val X : bool


do
    let _ = Foo(X = true)
    ()
"""

        this.AssertNoViolations()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/492
    [<Test>]
    member this.``Named parameter in atomic static method call should not be treated as infix operation``() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

type Foo() =
    static member Create(keepAssemblyContents: bool) = Foo()

do
    let foo = Foo.Create(keepAssemblyContents = true)
    ()
"""

        this.AssertNoViolations()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/492
    [<Test>]
    member this.``Named parameter in FSharpChecker.Create should not be treated as infix operation``() =
        this.SetConfig(["x = true ===> x"])

        this.Parse """
module Goat

open FSharp.Compiler.CodeAnalysis

do
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    ()
"""

        this.AssertNoViolations()

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

        this.SetConfig(["List.concat (List.map f x) ===> List.collect f x"])
        this.Parse(source)
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

        this.SetConfig(["if x then true else false ===> x"])
        this.Parse(source)
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

        this.SetConfig(["0 ===> foo 0"])
        this.Parse(source)
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

        this.SetConfig(["foo 0 ===> foo 0 0"])
        this.Parse(source)
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

        this.SetConfig(["bar x ===> id x"])
        this.Parse(source)
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

        this.SetConfig(["bar x ===> if x then 0 else 1"])
        this.Parse(source)
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

        this.SetConfig(["&x ===> &&x"])
        this.Parse(source)
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
        this.SetConfig(["- -x ===> x"])
        this.Parse(source)
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

        this.SetConfig(["List.map f (List.map g x) ===> List.map (g >> f) x"])
        this.Parse(source)
        Assert.AreEqual(expected, this.ApplyQuickFix source)