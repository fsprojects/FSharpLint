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
type TestHintOperators() =

    [<Test>]
    member this.Plus() = 
        match run Operators.poperator "+" with
            | Success(hint, _, _) -> Assert.AreEqual("+", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.PlusMinus() = 
        match run Operators.poperator "+-" with
            | Success(hint, _, _) -> Assert.AreEqual("+-", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintIdentifiers() =

    [<Test>]
    member this.Identifier() = 
        match run Identifiers.pident "duck" with
            | Success(hint, _, _) -> Assert.AreEqual("duck", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.BackTickedIdentifier() = 
        match run Identifiers.pident "``du+ck``" with
            | Success(hint, _, _) -> Assert.AreEqual("du+ck", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.LongIdent() = 
        match run Identifiers.plongident "Dog.``du+ck``.goats" with
            | Success(hint, _, _) -> Assert.AreEqual(["Dog";"du+ck";"goats"], hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.LongIdentWithOperator() = 
        match run Identifiers.plongidentorop "Dog.``du+ ck``.cat.(+)" with
            | Success(hint, _, _) -> Assert.AreEqual(["Dog";"du+ ck";"cat";"+"], hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.OperatorIdentifier() = 
        match run Identifiers.pidentorop "(+)" with
            | Success(hint, _, _) -> Assert.AreEqual("+", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.OperatorIdentifierWithWhitespace() = 
        match run Identifiers.pidentorop "(  +-?  )" with
            | Success(hint, _, _) -> Assert.AreEqual("+-?", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintStringAndCharacterLiterals() =

    [<Test>]
    member this.ByteCharacter() = 
        match run StringAndCharacterLiterals.pbytechar "'x'B" with
            | Success(hint, _, _) -> Assert.AreEqual('x'B, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.ByteArray() = 
        match run StringAndCharacterLiterals.pbytearray "\"dog\"B" with
            | Success(hint, _, _) -> Assert.AreEqual("dog"B, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Character() = 
        match run StringAndCharacterLiterals.pcharacter "'x'" with
            | Success(hint, _, _) -> Assert.AreEqual('x', hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.UnicodeCharacter() = 
        match run StringAndCharacterLiterals.pcharacter "'\\u0040'" with
            | Success(hint, _, _) -> Assert.AreEqual('@', hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.EscapeCharacter() = 
        match run StringAndCharacterLiterals.pcharacter "'\\n'" with
            | Success(hint, _, _) -> Assert.AreEqual('\n', hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.TrigraphCharacter() = 
        match run StringAndCharacterLiterals.pcharacter "'\\064'" with
            | Success(hint, _, _) -> Assert.AreEqual('@', hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.String() = 
        match run StringAndCharacterLiterals.pliteralstring "\"d\\tog\\064\\u0040\\U00000040goat\"" with
            | Success(hint, _, _) -> Assert.AreEqual("d\tog@@@goat", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintParserNumericLiterals() =

    [<Test>]
    member this.ByteMin() = 
        match run NumericLiterals.pbyte "0b0uy" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Byte.MinValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.ByteMax() = 
        match run NumericLiterals.pbyte "0b11111111uy" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Byte.MaxValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.SignedByteMax() = 
        match run NumericLiterals.psbyte "-128y" with
            | Success(hint, _, _) -> Assert.AreEqual(System.SByte.MinValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.SignedByteMin() = 
        match run NumericLiterals.psbyte "127y" with
            | Success(hint, _, _) -> Assert.AreEqual(System.SByte.MaxValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Int16Min() = 
        match run NumericLiterals.pint16 "-32768s" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Int16.MinValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Int16Max() = 
        match run NumericLiterals.pint16 "32767s" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Int16.MaxValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Uint16Min() = 
        match run NumericLiterals.puint16 "0us" with
            | Success(hint, _, _) -> Assert.AreEqual(System.UInt16.MinValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Uint16Max() = 
        match run NumericLiterals.puint16 "65535us" with
            | Success(hint, _, _) -> Assert.AreEqual(System.UInt16.MaxValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Int32Withl() = 
        match run NumericLiterals.pint32 "-2147483648l" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Int32.MinValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Int32Max() = 
        match run NumericLiterals.pint32 "2147483647" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Int32.MaxValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Uint32() = 
        match run NumericLiterals.puint32 "984u" with
            | Success(hint, _, _) -> Assert.AreEqual(984u, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Uint32Max() = 
        match run NumericLiterals.puint32 "0xFFFFFFFFu" with
            | Success(hint, _, _) -> Assert.AreEqual(System.UInt32.MaxValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Int64Min() = 
        match run NumericLiterals.pint64 "-9223372036854775808L" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Int64.MinValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Int64Max() = 
        match run NumericLiterals.pint64 "9223372036854775807L" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Int64.MaxValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Uint64() = 
        match run NumericLiterals.puint64 "984UL" with
            | Success(hint, _, _) -> Assert.AreEqual(984UL, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.Uint64Max() = 
        match run NumericLiterals.puint64 "0xFFFFFFFFFFFFFFFFuL" with
            | Success(hint, _, _) -> Assert.AreEqual(System.UInt64.MaxValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.SingleMax() = 
        match run NumericLiterals.psingle "3.40282347e+38f" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Single.MaxValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.SingleMin() = 
        match run NumericLiterals.psingle "-3.40282347e+38f" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Single.MinValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.DoubleLarge() = 
        match run NumericLiterals.pdouble "1.7E+308" with
            | Success(hint, _, _) -> Assert.AreEqual(1.7E+308, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.DoubleSmall() = 
        match run NumericLiterals.pdouble "-1.7E+308" with
            | Success(hint, _, _) -> Assert.AreEqual(-1.7E+308, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.BigNum() = 
        match run NumericLiterals.pbignum "1243124124124124124124214214124124124I" with
            | Success(hint, _, _) -> Assert.AreEqual(1243124124124124124124214214124124124I, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.FloatingDecimal() = 
        match run NumericLiterals.pdecimal "1.7E+10m" with
            | Success(hint, _, _) -> Assert.AreEqual(1.7E+10m, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.DecimalMax() = 
        match run NumericLiterals.pdecimal "79228162514264337593543950335m" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Decimal.MaxValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.DecimalMin() = 
        match run NumericLiterals.pdecimal "-79228162514264337593543950335m" with
            | Success(hint, _, _) -> Assert.AreEqual(System.Decimal.MinValue, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member this.DecimalInt() = 
        match run NumericLiterals.pdecimal "55m" with
            | Success(hint, _, _) -> Assert.AreEqual(55m, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

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