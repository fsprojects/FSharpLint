module FSharpLint.Core.Tests.Rules.Conventions.DisallowShadowing

open NUnit.Framework

open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsDisallowShadowing() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(DisallowShadowing.rule)

    [<Test>]
    member this.``Should produce error for shadowed variable``() =
        this.Parse """
let foo = 0

module Foo =
    let foo = 1"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable, inside function``() =
        this.Parse """
let bar () =
    let foo = 0
    let foo = 1
    foo"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (function argument)``() =
        this.Parse """
let foo = 0
let bar foo = foo + 1"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (function argument, inside function)``() =
        this.Parse """
let baz () =
    let foo = 0
    let bar foo = foo + 1
    ()"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (function argument, nested)``() =
        this.Parse """
let baz foo =
    let bar foo = foo + 1
    ()"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (function argument, tuple)``() =
        this.Parse """
let foo = 0
let bar (foo, baz) = foo + baz"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (lambda function argument)``() =
        this.Parse """
let foo = 0
(fun foo -> foo + 1) 0 |> ignore"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable inside lambda function``() =
        this.Parse """
(fun foo ->
    let foo = foo + 1
    foo) 0 |> ignore"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (match pattern)``() =
        this.Parse """
let foo = 0
match 1 with
| foo -> foo"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (as match pattern)``() =
        this.Parse """
let foo = 0
match (1, 2) with
| (x, y) as foo -> foo"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable inside match pattern``() =
        this.Parse """
match 1 with
| foo -> 
    let foo = foo + 1
    foo"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable inside type definition``() =
        this.Parse """
type Foo(foo) =
    let foo = foo + 1
    
    member this.Bar = foo"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable inside member definition``() =
        this.Parse """
type Foo() =
    member this.Bar(foo) = 
        let foo = foo + 1
        foo"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should not produce error when variable with same name exists in another module``() =
        this.Parse """
module Foo =
    let foo = 0
let foo = 1"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should not produce error when parameter with same name exists in another function``() =
        this.Parse """
let foo target =
    target

let bar target =
    target + 1"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should not produce error when variable name starts with underscore``() =
        this.Parse """
let _foo = 0

module Foo =
    let _foo = 1"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should not produce error for bindings in match patterns that are not shadowing``() =
        this.Parse """
match node with
| HintNode(expr, depth, rest) ->
    match (getKey expr, expr) with
    | (SyntaxHintNode.Wildcard as key), HintExpr(Expression.Wildcard)
    | (SyntaxHintNode.Variable as key), HintPat(Pattern.Variable(_)) -> Some(key, expr, depth, rest)
    | _ -> None
| EndOfHint(_) -> None"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``Should not produce error for vars used as args to active patterns``() =
        this.Parse """
let private (|RaiseWithTooManyArgs|_|) identifier maxArgs = function
    | ExpressionUtilities.Identifier([ ident ], _)::arguments 
        when List.length arguments > maxArgs && ident.idText = identifier ->
        Some()
    | _ -> None

let checkRaiseWithTooManyArgs (raiseType:string) (count:int) =
    match expressions with
    | RaiseWithTooManyArgs raiseType count -> Array.empty
    | _ -> Array.empty
"""

        Assert.IsTrue this.NoErrorsExist
