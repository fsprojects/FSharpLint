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

module TestHintParser

open NUnit.Framework
open FSharpLint.Framework.HintParser
open FParsec

[<TestFixture>]
type TestHintParser() =

    [<Test>]
    member this.Variable() = 
        match run pvariable "x " with
            | Success(hint, _, _) -> Assert.AreEqual(Variable('x'), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.ArgumentVariable() = 
        match run pargumentvariable "x " with
            | Success(hint, _, _) -> Assert.AreEqual(Argument.Variable('x'), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Bool() = 
        match run pbool "true" with
            | Success(hint, _, _) -> Assert.AreEqual(Bool(true), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.ConstantBool() = 
        match run pconstant "true" with
            | Success(hint, _, _) -> Assert.AreEqual(Constant(Bool(true)), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.ConstantFloat() = 
        match run pconstant "0.55" with
            | Success(hint, _, _) -> Assert.AreEqual(Constant(Float(0.55)), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.ConstantUnit() = 
        match run pconstant "()" with
            | Success(hint, _, _) -> Assert.AreEqual(Constant(Unit), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Wildcard() = 
        match run pwildcard "_" with
            | Success(hint, _, _) -> Assert.AreEqual(Wildcard, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.ArgumentWildcard() = 
        match run pargumentwildcard "_" with
            | Success(hint, _, _) -> Assert.AreEqual(Argument.Wildcard, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Identifier() = 
        match run pidentifier "not" with
            | Success(hint, _, _) -> Assert.AreEqual(Identifier(["not"]), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.LongIdentifier() = 
        match run pidentifier "List.map" with
            | Success(hint, _, _) -> Assert.AreEqual(Identifier(["List"; "map"]), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Unit() = 
        match run punit "()" with
            | Success(hint, _, _) -> Assert.AreEqual(Unit, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.SpacedUnit() = 
        match run punit "(   )" with
            | Success(hint, _, _) -> Assert.AreEqual(Unit, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.LambdaArguments() = 
        let expected =
            [
                Argument.Variable('x')
                Argument.Variable('y')
                Argument.Variable('g')
                Argument.Wildcard
                Argument.Variable('f')
            ]

        match run plambdaarguments "x y g _ f" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.ExpressionUnit() = 
        match run pexpression "(   )" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression([Constant(Unit)]), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.ExpressionParenAroundUnit() = 
        match run pexpression "((   ))" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression([Parentheses(Expression([Constant(Unit)]))]), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.ExpressionNotTrue() = 
        match run pexpression "not true" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression[Identifier(["not"]); Constant(Bool(true))], hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Lambda() = 
        let expected = 
            Lambda({
                    Arguments = [Argument.Variable('x'); Argument.Variable('y'); Argument.Wildcard]
                    Body = Expression([Parentheses(Expression([Variable('x'); Item.Operator(['+']); Variable('y')]))])
            })

        match run plambda "fun x y _ -> (x + y)" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.XEqualsXHint() = 
        let expected = 
            {
                Match = Expression[Variable('x')]
                Suggestion = Expression[Variable('x')]
            }

        match run phint "x ===> x" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.NotTrueIsFalseHint() = 
        let expected = 
            {
                Match = Expression[Identifier(["not"]); Constant(Bool(true))]
                Suggestion = Expression[Constant(Bool(false))]
            }

        match run phint "not true ===> false" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.FoldAddIntoSumHint() = 
        let expected = 
            {
                Match = Expression
                    [
                        Identifier(["List"; "fold"])
                        Parentheses(Expression([Item.Operator(['+'])]))
                        Constant(Float(0.0))
                        Variable('x')
                    ]
                Suggestion = Expression[Identifier(["List"; "sum"]); Variable('x')]
            }

        match run phint "List.fold (+) 0 x ===> List.sum x" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)