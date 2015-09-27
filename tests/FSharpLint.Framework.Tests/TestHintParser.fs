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
    member __.Plus() = 
        match run Operators.poperator "+" with
            | Success(hint, _, _) -> Assert.AreEqual("+", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.PlusMinus() = 
        match run Operators.poperator "+-" with
            | Success(hint, _, _) -> Assert.AreEqual("+-", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintIdentifiers() =

    [<Test>]
    member __.Identifier() = 
        match run Identifiers.plongidentorop "duck" with
            | Success(hint, _, _) -> Assert.AreEqual(["duck"], hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.BackTickedIdentifier() = 
        match run Identifiers.plongidentorop "``du+ck``" with
            | Success(hint, _, _) -> Assert.AreEqual(["du+ck"], hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.LongIdent() = 
        match run Identifiers.plongidentorop "Dog.``du+ck``.goats" with
            | Success(hint, _, _) -> Assert.AreEqual(["Dog";"du+ck";"goats"], hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.LongIdentWithOperator() = 
        match run Identifiers.plongidentorop "Dog.``du+ ck``.cat.(+)" with
            | Success(hint, _, _) -> Assert.AreEqual(["Dog";"du+ ck";"cat";"+"], hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.OperatorIdentifier() = 
        match run Identifiers.plongidentorop "(+)" with
            | Success(hint, _, _) -> Assert.AreEqual(["+"], hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.OperatorIdentifierWithWhitespace() = 
        match run Identifiers.plongidentorop "(  +-?  )" with
            | Success(hint, _, _) -> Assert.AreEqual(["+-?"], hint)
            | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintStringAndCharacterLiterals() =

    [<Test>]
    member __.ByteCharacter() = 
        match run StringAndCharacterLiterals.pbytechar "'x'B" with
            | Success(hint, _, _) -> Assert.AreEqual(Byte('x'B), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ByteArray() = 
        match run StringAndCharacterLiterals.pbytearray "\"dog\"B" with
            | Success(hint, _, _) -> Assert.AreEqual(Bytes("dog"B), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Character() = 
        match run StringAndCharacterLiterals.pcharacter "'x'" with
            | Success(hint, _, _) -> Assert.AreEqual(Char('x'), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.UnicodeCharacter() = 
        match run StringAndCharacterLiterals.pcharacter "'\\u0040'" with
            | Success(hint, _, _) -> Assert.AreEqual(Char('@'), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.EscapeCharacter() = 
        match run StringAndCharacterLiterals.pcharacter "'\\n'" with
            | Success(hint, _, _) -> Assert.AreEqual(Char('\n'), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.TrigraphCharacter() = 
        match run StringAndCharacterLiterals.pcharacter "'\\064'" with
            | Success(hint, _, _) -> Assert.AreEqual(Char('@'), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.StringIncludingSingleQuote() = 
        match run StringAndCharacterLiterals.pliteralstring "\"'\"" with
            | Success(hint, _, _) -> Assert.AreEqual("'", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.String() = 
        match run StringAndCharacterLiterals.pliteralstring "\"d\\tog\\064\\u0040\\U00000040goat\"" with
            | Success(hint, _, _) -> Assert.AreEqual("d\tog@@@goat", hint)
            | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintParserNumericLiterals() =

    [<Test>]
    member __.ByteMin() = 
        match run NumericLiterals.pbyte "0b0uy" with
            | Success(hint, _, _) -> Assert.AreEqual(Byte(System.Byte.MinValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ByteMax() = 
        match run NumericLiterals.pbyte "0b11111111uy" with
            | Success(hint, _, _) -> Assert.AreEqual(Byte(System.Byte.MaxValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.SignedByteMax() = 
        match run NumericLiterals.psbyte "-128y" with
            | Success(hint, _, _) -> Assert.AreEqual(SByte(System.SByte.MinValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.SignedByteMin() = 
        match run NumericLiterals.psbyte "127y" with
            | Success(hint, _, _) -> Assert.AreEqual(SByte(System.SByte.MaxValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Int16Min() = 
        match run NumericLiterals.pint16 "-32768s" with
            | Success(hint, _, _) -> Assert.AreEqual(Int16(System.Int16.MinValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Int16Max() = 
        match run NumericLiterals.pint16 "32767s" with
            | Success(hint, _, _) -> Assert.AreEqual(Int16(System.Int16.MaxValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Uint16Min() = 
        match run NumericLiterals.puint16 "0us" with
            | Success(hint, _, _) -> Assert.AreEqual(UInt16(System.UInt16.MinValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Uint16Max() = 
        match run NumericLiterals.puint16 "65535us" with
            | Success(hint, _, _) -> Assert.AreEqual(UInt16(System.UInt16.MaxValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Int32Withl() = 
        match run NumericLiterals.pint32 "-2147483648l" with
            | Success(hint, _, _) -> Assert.AreEqual(Int32(System.Int32.MinValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Int32Max() = 
        match run NumericLiterals.pint32 "2147483647" with
            | Success(hint, _, _) -> Assert.AreEqual(Int32(System.Int32.MaxValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Uint32() = 
        match run NumericLiterals.puint32 "984u" with
            | Success(hint, _, _) -> Assert.AreEqual(UInt32(984u), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Uint32Max() = 
        match run NumericLiterals.puint32 "0xFFFFFFFFu" with
            | Success(hint, _, _) -> Assert.AreEqual(UInt32(System.UInt32.MaxValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Int64Min() = 
        match run NumericLiterals.pint64 "-9223372036854775808L" with
            | Success(hint, _, _) -> Assert.AreEqual(Int64(System.Int64.MinValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Int64Max() = 
        match run NumericLiterals.pint64 "9223372036854775807L" with
            | Success(hint, _, _) -> Assert.AreEqual(Int64(System.Int64.MaxValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Uint64() = 
        match run NumericLiterals.puint64 "984UL" with
            | Success(hint, _, _) -> Assert.AreEqual(UInt64(984UL), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Uint64Max() = 
        match run NumericLiterals.puint64 "0xFFFFFFFFFFFFFFFFuL" with
            | Success(hint, _, _) -> Assert.AreEqual(UInt64(System.UInt64.MaxValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.SingleMax() = 
        match run NumericLiterals.psingle "3.40282347e+38f" with
            | Success(hint, _, _) -> Assert.AreEqual(Single(System.Single.MaxValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.SingleMin() = 
        match run NumericLiterals.psingle "-3.40282347e+38f" with
            | Success(hint, _, _) -> Assert.AreEqual(Single(System.Single.MinValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.DoubleLarge() = 
        match run NumericLiterals.pdouble "1.7E+308" with
            | Success(hint, _, _) -> Assert.AreEqual(Double(1.7E+308), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.DoubleSmall() = 
        match run NumericLiterals.pdouble "-1.7E+308" with
            | Success(hint, _, _) -> Assert.AreEqual(Double(-1.7E+308), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.BigNum() = 
        match run NumericLiterals.pbignum "1243124124124124124124214214124124124I" with
            | Success(hint, _, _) -> Assert.AreEqual(UserNum(1243124124124124124124214214124124124I, 'I'), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.FloatingDecimal() = 
        match run NumericLiterals.pdecimal "1.7E+10m" with
            | Success(hint, _, _) -> Assert.AreEqual(Decimal(1.7E+10m), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.DecimalMax() = 
        match run NumericLiterals.pdecimal "79228162514264337593543950335m" with
            | Success(hint, _, _) -> Assert.AreEqual(Decimal(System.Decimal.MaxValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.DecimalMin() = 
        match run NumericLiterals.pdecimal "-79228162514264337593543950335m" with
            | Success(hint, _, _) -> Assert.AreEqual(Decimal(System.Decimal.MinValue), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.DecimalInt() = 
        match run NumericLiterals.pdecimal "55m" with
            | Success(hint, _, _) -> Assert.AreEqual(Decimal(55m), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestConstantParser() =

    [<Test>]
    member __.Bool() = 
        match run Constants.pconstant "true" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Constant(Bool(true)), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Unit() = 
        match run Constants.pconstant "()" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Constant(Unit), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.LiteralString() = 
        match run Constants.pconstant "\"dog\"" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Constant(String("dog")), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Int16() = 
        match run Constants.pconstant "14s" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Constant(Int16(14s)), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Int32() = 
        match run Constants.pconstant "14" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Constant(Int32(14)), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Double() = 
        match run Constants.pconstant "14.1" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Constant(Double(14.1)), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Decimal() = 
        match run Constants.pconstant "14.1m" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Constant(Decimal(14.1m)), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintParser() =

    [<Test>]
    member __.Variable() = 
        match run Expressions.pvariable "x " with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Variable('x'), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ArgumentVariable() = 
        match run Expressions.pargumentvariable "x " with
            | Success(hint, _, _) -> Assert.AreEqual(Argument.Variable('x'), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Wildcard() = 
        match run Expressions.pwildcard "_" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Wildcard, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ArgumentWildcard() = 
        match run Expressions.pargumentwildcard "_" with
            | Success(hint, _, _) -> Assert.AreEqual(Argument.Wildcard, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.LambdaArguments() = 
        let expected =
            [
                Argument.Variable('x')
                Argument.Variable('y')
                Argument.Variable('g')
                Argument.Wildcard
                Argument.Variable('f')
            ]

        match run Expressions.plambdaarguments "x y g _ f" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ExpressionIdentifier() = 
        match run Expressions.pexpression "id" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Identifier(["id"]), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.``Keyword should not be parsed as identifier.``() = 
        match run Expressions.pexpression "fun" with
            | Success(_) -> Assert.Fail()
            | Failure(_) -> Assert.Pass()

    [<Test>]
    member __.``Keyword in back ticks should be parsed as identifier.``() = 
        match run Expressions.pexpression "``fun``" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Identifier(["fun"]), hint)
            | Failure(_) -> Assert.Fail()

    [<Test>]
    member __.ExpressionUnit() = 
        match run Expressions.pexpression "(   )" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Constant(Unit), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ExpressionParenAroundUnit() = 
        match run Expressions.pexpression "((   ))" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.Parentheses(Expression.Constant(Unit)), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ExpressionAdd() = 
        match run Expressions.pexpression "4+5" with
            | Success(hint, _, _) -> Assert.AreEqual(
                                        Expression.InfixOperator("+", 
                                            Expression.Constant(Constant.Int32(4)), Expression.Constant(Constant.Int32(5))), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ExpressionAddAndMultiply() = 
        let expected =
            Expression.InfixOperator("+",
                Expression.InfixOperator("*", 
                    Expression.Constant(Constant.Int32(4)), Expression.Constant(Constant.Int32(5))),
                Expression.InfixOperator("*", 
                    Expression.Variable('x'), Expression.Constant(Constant.Int32(3))))

        match run Expressions.pexpression "4*5+x*3" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ExpressionPrefixOperator() = 
        let expected = Expression.PrefixOperator("&", Expression.Constant(Constant.Int32(4)))

        match run Expressions.pexpression "&4" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ExpressionPrefixAddition() = 
        let expected =
            Expression.InfixOperator("+",
                Expression.Constant(Constant.Int32(4)),
                Expression.PrefixOperator("+", Expression.Constant(Constant.Int32(4))))

        match run Expressions.pexpression "4 + +4" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ExpressionBracketedAddAndMultiply() = 
        let expected =
            Expression.InfixOperator("*",
                Expression.Constant(Constant.Int32(4)),
                Expression.Parentheses(
                    Expression.InfixOperator("+", 
                        Expression.Constant(Constant.Int32(5)), Expression.Variable('x'))))

        match run Expressions.pexpression "4 * (5 + x)" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.ExpressionNotTrue() = 
        match run Expressions.pexpression "not true" with
            | Success(hint, _, _) -> Assert.AreEqual(Expression.FunctionApplication(
                                                        [
                                                            Expression.Identifier(["not"])
                                                            Expression.Constant(Bool(true))
                                                        ]), hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Lambda() = 
        let expected = 
            Expression.Lambda(
                {
                    Arguments = [Argument.Variable('x'); Argument.Variable('y'); Argument.Wildcard]
                    Body = Expression.Parentheses(Expression.InfixOperator("+", Expression.Variable('x'), Expression.Variable('y')))
                })

        match run Expressions.plambda "fun x y _ -> (x + y)" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.XEqualsXHint() = 
        let expected = 
            {
                Match = Expression.Variable('x')
                Suggestion = Suggestion.Expr(Expression.Variable('x'))
            }

        match run phint "x ===> x" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.NotTrueIsFalseHint() = 
        let expected = 
            {
                Match = Expression.FunctionApplication
                    [
                        Expression.Identifier(["not"])
                        Expression.Constant(Bool(true))
                    ]
                Suggestion = Suggestion.Expr(Expression.Constant(Bool(false)))
            }

        match run phint "not true ===> false" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)
            
    [<Test>]
    member __.FoldAddIntoSumHint() = 
        let expected = 
            {
                Match = Expression.FunctionApplication
                    [ Expression.Identifier(["List"; "fold"])
                      Expression.Identifier(["+"])
                      Expression.Constant(Int32(0))
                      Expression.Variable('x') ]
                Suggestion = Suggestion.Expr(
                                Expression.FunctionApplication(
                                    [ Expression.Identifier(["List"; "sum"])
                                      Expression.Variable('x') ]))
            }

        match run phint "List.fold (+) 0 x ===> List.sum x" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)
            
    [<Test>]
    member __.IdHint() = 
        let expected = 
            {
                Match = Expression.Lambda(
                            { Arguments = [Argument.Variable('x')]
                              Body = Expression.Variable('x') })
                Suggestion = Suggestion.Expr(Expression.Identifier(["id"]))
            }

        match run phint "fun x -> x ===> id" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Tuple() = 
        let expected = Expression.Tuple([Expression.Variable('x');Expression.Variable('y')])

        match run Expressions.ptuple "(x, y)" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.List() = 
        let expected = Expression.List([Expression.Variable('x');Expression.Variable('y')])

        match run Expressions.plist "[x; y]" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.EmptyList() = 
        let expected = Expression.List([])

        match run Expressions.plist "[]" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)
           
    [<Test>]
    member __.EmptyListWithWhitespace() = 
        let expected = 
            Expression.List([])

        match run Expressions.plist "[  ]" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.Array() = 
        let expected = Expression.Array([Expression.Variable('x');Expression.Variable('y')])

        match run Expressions.parray "[|x; y|]" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.EmptyArray() = 
        let expected = Expression.Array([])

        match run Expressions.parray "[||]" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)
           
    [<Test>]
    member __.EmptyArrayWithWhitespace() = 
        let expected = 
            Expression.Array([])

        match run Expressions.parray "[|  |]" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)
            
    [<Test>]
    member __.TupleHint() = 
        let expected = 
            {
                Match = Expression.Tuple([Expression.Variable('x');Expression.Variable('y')])
                Suggestion = Suggestion.Expr(Expression.Identifier(["id"]))
            }
            
        match run phint "(x, y) ===> id" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)
            
    [<Test>]
    member __.TupleInFunctionApplication() = 
        let expected = 
            {
                Match = Expression.FunctionApplication(
                            [ Expression.Identifier(["fst"])
                              Expression.Tuple([Expression.Variable('x');Expression.Variable('y')]) ])
                Suggestion = Suggestion.Expr(Expression.Variable('x'))
            }
            
        match run phint "fst (x, y) ===> x" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)
            
    [<Test>]
    member __.ListHint() = 
        let expected =
            {
                Match = Expression.InfixOperator("::", Expression.Variable('x'), Expression.List([]))
                Suggestion = Suggestion.Expr(Expression.List([Expression.Variable('x')]))
            }
            
        match run phint "x::[] ===> [x]" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)
            
    [<Test>]
    member __.ArrayHint() = 
        let expected =
            {
                Match = Expression.Array([Expression.Variable('x')])
                Suggestion = Suggestion.Expr(Expression.Variable('x'))
            }
            
        match run phint "[|x|] ===> x" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)
            
    [<Test>]
    member __.IfStatementHint() =
        let expected =
            {
                Match = Expression.If(
                            Expression.Variable('x'),
                            Expression.Constant(Constant.Bool(true)),
                            Some(Expression.Constant(Constant.Bool(false))))
                Suggestion = Suggestion.Expr(Expression.Variable('x'))
            }
            
        match run phint "if x then true else false ===> x" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.MultipleFunctionApplicationsHint() = 
        let expected = 
            {
                Match = Expression.FunctionApplication(
                            [ Expression.Identifier(["List";"head"])
                              Expression.Parentheses(
                                  Expression.FunctionApplication(
                                      [ Expression.Identifier(["List";"sort"])
                                        Expression.Variable('x') ])) ])
                Suggestion = Suggestion.Expr(
                                Expression.FunctionApplication(
                                    [ Expression.Identifier(["List";"min"])
                                      Expression.Variable('x') ]))
            }

        match run phint "List.head (List.sort x) ===> List.min x" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.``Suggestion with literal string message parsed correctly.``() = 
        let expected = 
            { Match = Expression.Constant(Constant.Unit)
              Suggestion = Suggestion.Message("Message") }

        match run phint "() ===> m\"Message\"" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.``Suggestion with verbatim literal string message parsed correctly.``() = 
        let expected = 
            { Match = Expression.Constant(Constant.Unit)
              Suggestion = Suggestion.Message("Message") }

        match run phint "() ===> m@\"Message\"" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.``Suggestion with triple quoted string message parsed correctly.``() = 
        let expected = 
            { Match = Expression.Constant(Constant.Unit)
              Suggestion = Suggestion.Message("Message") }

        match run phint "() ===> m\"\"\"Message\"\"\"" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member __.``Parses null into a null expression.``() = 
        let expected = 
            { Match = Expression.Null
              Suggestion = Suggestion.Message("Message") }

        match run phint "null ===> m\"\"\"Message\"\"\"" with
            | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
            | Failure(message, _, _) -> Assert.Fail(message)