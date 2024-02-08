module FSharpLint.Core.Tests.TestHintParser

// fsharplint:disable TupleIndentation

open NUnit.Framework
open FSharpLint.Framework
open FSharpLint.Framework.HintParser
open FParsec
open FSharpLint.Framework.MergeSyntaxTrees
open FSharpLint.Framework.HintParserTypes
open System.Collections.Generic

[<TestFixture>]
type TestMergeSyntaxTrees() =

    [<Test>]
    member _.``Merge two function applications of same function with diff arguments, merged list has common prefix till args.``() =
        let toDictionary list =
            let dictionary = Dictionary<int, Node>()
            for (x, y) in list do dictionary.Add(x, y)
            dictionary

        match (run phint "List.map id ===> id", run phint "List.map woof ===> woof") with
        | Success(hint, _, _), Success(hint2, _, _) ->
            let expectedEdges =
                { Lookup =
                    [ (Utilities.hash2 SyntaxHintNode.Identifier "map",
                       { Edges =
                           { Lookup =
                               [ (Utilities.hash2 SyntaxHintNode.Identifier "woof",
                                  { Edges = Edges.Empty
                                    MatchedHint = [hint2] })
                                 (Utilities.hash2 SyntaxHintNode.Identifier "id",
                                  { Edges = Edges.Empty
                                    MatchedHint = [hint] }) ] |> toDictionary
                             AnyMatch = [] }
                         MatchedHint = [] }) ] |> toDictionary
                  AnyMatch = [] }

            let expectedRoot =
                { Lookup =
                    [(Utilities.hash2 SyntaxHintNode.FuncApp 0,
                      { Edges = expectedEdges; MatchedHint = [] })] |> toDictionary
                  AnyMatch = [] }

            let mergedHint = MergeSyntaxTrees.mergeHints [hint; hint2]

            Assert.AreEqual(expectedRoot, mergedHint)
        | _ -> Assert.Fail()

    [<Test>]
    member _.``Merge no hints gives no merged lists``() =
        Assert.AreEqual(MergeSyntaxTrees.Edges.Empty, MergeSyntaxTrees.mergeHints [])

[<TestFixture>]
type TestHintOperators() =

    [<Test>]
    member _.Plus() =
        match run Operators.poperator "+" with
        | Success(hint, _, _) -> Assert.AreEqual("+", hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.PlusMinus() =
        match run Operators.poperator "+-" with
        | Success(hint, _, _) -> Assert.AreEqual("+-", hint)
        | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintIdentifiers() =

    [<Test>]
    member _.Identifier() =
        match run Identifiers.plongidentorop "duck" with
        | Success(hint, _, _) -> Assert.AreEqual(["duck"], hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.BackTickedIdentifier() =
        match run Identifiers.plongidentorop "``du+ck``" with
        | Success(hint, _, _) -> Assert.AreEqual(["du+ck"], hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.LongIdent() =
        match run Identifiers.plongidentorop "Dog.``du+ck``.goats" with
        | Success(hint, _, _) -> Assert.AreEqual(["Dog";"du+ck";"goats"], hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.LongIdentWithOperator() =
        match run Identifiers.plongidentorop "Dog.``du+ ck``.cat.(+)" with
        | Success(hint, _, _) -> Assert.AreEqual(["Dog";"du+ ck";"cat";"+"], hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.OperatorIdentifier() =
        match run Identifiers.plongidentorop "(+)" with
        | Success(hint, _, _) -> Assert.AreEqual(["+"], hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.OperatorIdentifierWithWhitespace() =
        match run Identifiers.plongidentorop "(  +-?  )" with
        | Success(hint, _, _) -> Assert.AreEqual(["+-?"], hint)
        | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintStringAndCharacterLiterals() =

    [<Test>]
    member _.ByteCharacter() =
        match run StringAndCharacterLiterals.pbytechar "'x'B" with
        | Success(hint, _, _) -> Assert.AreEqual(Byte('x'B), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ByteArray() =
        match run StringAndCharacterLiterals.pbytearray "\"dog\"B" with
        | Success(hint, _, _) -> Assert.AreEqual(Bytes("dog"B), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Character() =
        match run StringAndCharacterLiterals.pcharacter "'x'" with
        | Success(hint, _, _) -> Assert.AreEqual(Char('x'), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.UnicodeCharacter() =
        match run StringAndCharacterLiterals.pcharacter "'\\u0040'" with
        | Success(hint, _, _) -> Assert.AreEqual(Char('@'), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.EscapeCharacter() =
        match run StringAndCharacterLiterals.pcharacter "'\\n'" with
        | Success(hint, _, _) -> Assert.AreEqual(Char('\n'), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.TrigraphCharacter() =
        match run StringAndCharacterLiterals.pcharacter "'\\064'" with
        | Success(hint, _, _) -> Assert.AreEqual(Char('@'), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.StringIncludingSingleQuote() =
        match run StringAndCharacterLiterals.pliteralstring "\"'\"" with
        | Success(hint, _, _) -> Assert.AreEqual("'", hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.String() =
        match run StringAndCharacterLiterals.pliteralstring "\"d\\tog\\064\\u0040\\U00000040goat\"" with
        | Success(hint, _, _) -> Assert.AreEqual("d\tog@@@goat", hint)
        | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintParserNumericLiterals() =

    [<Test>]
    member _.ByteMin() =
        match run NumericLiterals.pbyte "0b0uy" with
        | Success(hint, _, _) -> Assert.AreEqual(Byte(System.Byte.MinValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ByteMax() =
        match run NumericLiterals.pbyte "0b11111111uy" with
        | Success(hint, _, _) -> Assert.AreEqual(Byte(System.Byte.MaxValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.SignedByteMax() =
        match run NumericLiterals.psbyte "-128y" with
        | Success(hint, _, _) -> Assert.AreEqual(SByte(System.SByte.MinValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.SignedByteMin() =
        match run NumericLiterals.psbyte "127y" with
        | Success(hint, _, _) -> Assert.AreEqual(SByte(System.SByte.MaxValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Int16Min() =
        match run NumericLiterals.pint16 "-32768s" with
        | Success(hint, _, _) -> Assert.AreEqual(Int16(System.Int16.MinValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Int16Max() =
        match run NumericLiterals.pint16 "32767s" with
        | Success(hint, _, _) -> Assert.AreEqual(Int16(System.Int16.MaxValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Uint16Min() =
        match run NumericLiterals.puint16 "0us" with
        | Success(hint, _, _) -> Assert.AreEqual(UInt16(System.UInt16.MinValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Uint16Max() =
        match run NumericLiterals.puint16 "65535us" with
        | Success(hint, _, _) -> Assert.AreEqual(UInt16(System.UInt16.MaxValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Int32Withl() =
        match run NumericLiterals.pint32 "-2147483648l" with
        | Success(hint, _, _) -> Assert.AreEqual(Int32(System.Int32.MinValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Int32Max() =
        match run NumericLiterals.pint32 "2147483647" with
        | Success(hint, _, _) -> Assert.AreEqual(Int32(System.Int32.MaxValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Uint32() =
        match run NumericLiterals.puint32 "984u" with
        | Success(hint, _, _) -> Assert.AreEqual(UInt32(984u), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Uint32Max() =
        match run NumericLiterals.puint32 "0xFFFFFFFFu" with
        | Success(hint, _, _) -> Assert.AreEqual(UInt32(System.UInt32.MaxValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Int64Min() =
        match run NumericLiterals.pint64 "-9223372036854775808L" with
        | Success(hint, _, _) -> Assert.AreEqual(Int64(System.Int64.MinValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Int64Max() =
        match run NumericLiterals.pint64 "9223372036854775807L" with
        | Success(hint, _, _) -> Assert.AreEqual(Int64(System.Int64.MaxValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Uint64() =
        match run NumericLiterals.puint64 "984UL" with
        | Success(hint, _, _) -> Assert.AreEqual(UInt64(984UL), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Uint64Max() =
        match run NumericLiterals.puint64 "0xFFFFFFFFFFFFFFFFuL" with
        | Success(hint, _, _) -> Assert.AreEqual(UInt64(System.UInt64.MaxValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.SingleMax() =
        match run NumericLiterals.psingle "3.40282347e+38f" with
        | Success(hint, _, _) -> Assert.AreEqual(Single(System.Single.MaxValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.SingleMin() =
        match run NumericLiterals.psingle "-3.40282347e+38f" with
        | Success(hint, _, _) -> Assert.AreEqual(Single(System.Single.MinValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.DoubleLarge() =
        match run NumericLiterals.pdouble "1.7E+308" with
        | Success(hint, _, _) -> Assert.AreEqual(Double(1.7E+308), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.DoubleSmall() =
        match run NumericLiterals.pdouble "-1.7E+308" with
        | Success(hint, _, _) -> Assert.AreEqual(Double(-1.7E+308), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.BigNum() =
        match run NumericLiterals.pbignum "1243124124124124124124214214124124124I" with
        | Success(hint, _, _) -> Assert.AreEqual(UserNum(1243124124124124124124214214124124124I, 'I'), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.FloatingDecimal() =
        match run NumericLiterals.pdecimal "1.7E+10m" with
        | Success(hint, _, _) -> Assert.AreEqual(Decimal(1.7E+10m), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.DecimalMax() =
        match run NumericLiterals.pdecimal "79228162514264337593543950335m" with
        | Success(hint, _, _) -> Assert.AreEqual(Decimal(System.Decimal.MaxValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.DecimalMin() =
        match run NumericLiterals.pdecimal "-79228162514264337593543950335m" with
        | Success(hint, _, _) -> Assert.AreEqual(Decimal(System.Decimal.MinValue), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.DecimalInt() =
        match run NumericLiterals.pdecimal "55m" with
        | Success(hint, _, _) -> Assert.AreEqual(Decimal(55m), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestConstantParser() =

    [<Test>]
    member _.Bool() =
        match run Constants.pconstant "true" with
        | Success(hint, _, _) -> Assert.AreEqual(Bool(true), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Unit() =
        match run Constants.pconstant "()" with
        | Success(hint, _, _) -> Assert.AreEqual(Unit, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.LiteralString() =
        match run Constants.pconstant "\"dog\"" with
        | Success(hint, _, _) -> Assert.AreEqual(String("dog"), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Int16() =
        match run Constants.pconstant "14s" with
        | Success(hint, _, _) -> Assert.AreEqual(Int16(14s), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Int32() =
        match run Constants.pconstant "14" with
        | Success(hint, _, _) -> Assert.AreEqual(Int32(14), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Double() =
        match run Constants.pconstant "14.1" with
        | Success(hint, _, _) -> Assert.AreEqual(Double(14.1), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Decimal() =
        match run Constants.pconstant "14.1m" with
        | Success(hint, _, _) -> Assert.AreEqual(Decimal(14.1m), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

[<TestFixture>]
type TestHintParser() =

    [<Test>]
    member _.Variable() =
        match run Expressions.pvariable "x " with
        | Success(hint, _, _) -> Assert.AreEqual(Expression.Variable('x'), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ArgumentVariable() =
        match run Expressions.pargumentvariable "x " with
        | Success(hint, _, _) -> Assert.AreEqual(Expression.Variable('x'), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Wildcard() =
        match run Expressions.pwildcard "_" with
        | Success(hint, _, _) -> Assert.AreEqual(Expression.Wildcard, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.LambdaArguments() =
        let expected =
            [ Expression.Variable('x')
              Expression.Variable('y')
              Expression.Variable('g')
              Expression.Wildcard
              Expression.Variable('f') ]

        match run Expressions.plambdaarguments "x y g _ f" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ExpressionIdentifier() =
        match run Expressions.pexpression "id" with
        | Success(hint, _, _) -> Assert.AreEqual(Expression.Identifier(["id"]), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Keyword should not be parsed as identifier.``() =
        match run Expressions.pexpression "fun" with
        | Success(_) -> Assert.Fail()
        | Failure(_) -> Assert.Pass()

    [<Test>]
    member _.``Keyword in back ticks should be parsed as identifier.``() =
        match run Expressions.pexpression "``fun``" with
        | Success(hint, _, _) -> Assert.AreEqual(Expression.Identifier(["fun"]), hint)
        | Failure(_) -> Assert.Fail()

    [<Test>]
    member _.ExpressionUnit() =
        match run Expressions.pexpression "(   )" with
        | Success(hint, _, _) -> Assert.AreEqual(Expression.Constant(Unit), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ExpressionParenAroundUnit() =
        match run Expressions.pexpression "((   ))" with
        | Success(hint, _, _) -> Assert.AreEqual(Expression.Parentheses(Expression.Constant(Unit)), hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ExpressionAdd() =
        match run Expressions.pexpression "4+5" with
        | Success(hint, _, _) ->
            Assert.AreEqual(
                Expression.InfixOperator(
                    Expression.Identifier(["+"]),
                    Expression.Constant(Constant.Int32(4)),
                    Expression.Constant(Constant.Int32(5))),
                hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ExpressionAddAndMultiply() =
        let expected =
            Expression.InfixOperator(
                Expression.Identifier(["+"]),
                Expression.InfixOperator(Expression.Identifier(["*"]),
                    Expression.Constant(Constant.Int32(4)), Expression.Constant(Constant.Int32(5))),
                Expression.InfixOperator(Expression.Identifier(["*"]),
                    Expression.Variable('x'), Expression.Constant(Constant.Int32(3))))

        match run Expressions.pexpression "4*5+x*3" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ExpressionPrefixOperator() =
        let expected = Expression.PrefixOperator(Expression.Identifier(["~"]), Expression.Constant(Constant.Int32(4)))

        match run Expressions.pexpression "~4" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Prefix operator of multiple tildes parsed correctly``() =
        let expected = Expression.PrefixOperator(Expression.Identifier(["~~~"]), Expression.Constant(Constant.Int32(4)))

        match run Expressions.pexpression "~~~4" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Bang prefix operator parsed correctly``() =
        let expected = Expression.PrefixOperator(Expression.Identifier(["!!!/<=>@!!!*"]), Expression.Constant(Constant.Int32(4)))

        match run Expressions.pexpression "!!!/<=>@!!!*4" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ExpressionPrefixAddition() =
        let expected =
            Expression.InfixOperator(Expression.Identifier(["+"]),
                Expression.Constant(Constant.Int32(4)),
                Expression.PrefixOperator(Expression.Identifier(["~+"]), Expression.Constant(Constant.Int32(4))))

        match run Expressions.pexpression "4 + +4" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ExpressionBracketedAddAndMultiply() =
        let expected =
            Expression.InfixOperator(Expression.Identifier(["*"]),
                Expression.Constant(Constant.Int32(4)),
                Expression.Parentheses(
                    Expression.InfixOperator(Expression.Identifier(["+"]),
                        Expression.Constant(Constant.Int32(5)), Expression.Variable('x'))))

        match run Expressions.pexpression "4 * (5 + x)" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ExpressionNotTrue() =
        match run Expressions.pexpression "not true" with
        | Success(hint, _, _) ->
            Assert.AreEqual(
                Expression.FunctionApplication(
                    [ Expression.Identifier(["not"])
                      Expression.Constant(Bool(true)) ]),
                hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Lambda() =
        let expected =
            Expression.Lambda(
                [ LambdaArg(Expression.LambdaArg(Expression.Variable('x')))
                  LambdaArg(Expression.LambdaArg(Expression.Variable('y')))
                  LambdaArg(Expression.LambdaArg(Expression.Wildcard))],
                LambdaBody(
                    Expression.LambdaBody(
                        Expression.Parentheses(
                            Expression.InfixOperator(
                                Expression.Identifier(["+"]),
                                Expression.Variable('x'),
                                Expression.Variable('y'))))))

        match run Expressions.plambda "fun x y _ -> (x + y)" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.XEqualsXHint() =
        let expected =
            { MatchedNode = Expression.Variable('x') |> HintExpr
              Suggestion = Suggestion.Expr(Expression.Variable('x')) }

        match run phint "x ===> x" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.NotTrueIsFalseHint() =
        let expected =
            { MatchedNode = Expression.FunctionApplication
                  [ Expression.Identifier(["not"])
                    Expression.Constant(Bool(true)) ] |> HintExpr
              Suggestion = Suggestion.Expr(Expression.Constant(Bool(false))) }

        match run phint "not true ===> false" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.FoldAddIntoSumHint() =
        let expected =
            { MatchedNode = Expression.FunctionApplication
                        [ Expression.Identifier(["List"; "fold"])
                          Expression.Identifier(["+"])
                          Expression.Constant(Int32(0))
                          Expression.Variable('x') ] |> HintExpr
              Suggestion = Suggestion.Expr(
                                Expression.FunctionApplication(
                                    [ Expression.Identifier(["List"; "sum"])
                                      Expression.Variable('x') ])) }

        match run phint "List.fold (+) 0 x ===> List.sum x" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.MapToSumIntoSumByHint() =
        let expected =
            { MatchedNode = Expression.FunctionApplication
                        [ Expression.Identifier(["List"; "sum"])
                          Expression.Parentheses(
                              Expression.FunctionApplication(
                                  [ Expression.Identifier(["List"; "map"])
                                    Expression.Variable('x')
                                    Expression.Variable('y') ])) ] |> HintExpr
              Suggestion = Suggestion.Expr(
                                Expression.FunctionApplication(
                                    [ Expression.Identifier(["List"; "sumBy"])
                                      Expression.Variable('x')
                                      Expression.Variable('y') ])) }

        match run phint "List.sum (List.map x y) ===> List.sumBy x y" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.MapToAverageIntoAverageByHint() =
        let expected =
            { MatchedNode = Expression.FunctionApplication
                        [ Expression.Identifier(["List"; "average"])
                          Expression.Parentheses(
                              Expression.FunctionApplication(
                                  [ Expression.Identifier(["List"; "map"])
                                    Expression.Variable('x')
                                    Expression.Variable('y') ])) ] |> HintExpr
              Suggestion = Suggestion.Expr(
                                Expression.FunctionApplication(
                                    [ Expression.Identifier(["List"; "averageBy"])
                                      Expression.Variable('x')
                                      Expression.Variable('y') ])) }

        match run phint "List.average (List.map x y) ===> List.averageBy x y" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.TupleTakeAndSkipIntoSplitAtHint() =
        let expected =
            { MatchedNode =
                Expression.Tuple(
                    [ Expression.FunctionApplication(
                         [ Expression.Identifier(["List"; "take"])
                           Expression.Variable('x')
                           Expression.Variable('y') ])
                      Expression.FunctionApplication(
                         [ Expression.Identifier(["List"; "skip"])
                           Expression.Variable('x')
                           Expression.Variable('y') ]) ]) |> HintExpr
              Suggestion = Suggestion.Expr(
                                Expression.FunctionApplication(
                                    [ Expression.Identifier(["List"; "splitAt"])
                                      Expression.Variable('x')
                                      Expression.Variable('y') ])) }

        match run phint "(List.take x y, List.skip x y) ===> List.splitAt x y" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.IdHint() =
        let expected =
            { MatchedNode = Expression.Lambda
                ([LambdaArg(Expression.LambdaArg(Expression.Variable('x')))],
                 LambdaBody(Expression.LambdaBody(Expression.Variable('x')))) |> HintExpr
              Suggestion = Suggestion.Expr(Expression.Identifier(["id"])) }

        match run phint "fun x -> x ===> id" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Tuple() =
        let expected = Expression.Tuple([Expression.Variable('x');Expression.Variable('y')])

        match run Expressions.ptuple "(x, y)" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.List() =
        let expected = Expression.List([Expression.Variable('x');Expression.Variable('y')])

        match run Expressions.plist "[x; y]" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.EmptyList() =
        let expected = Expression.List([])

        match run Expressions.plist "[]" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.EmptyListWithWhitespace() =
        let expected =
            Expression.List([])

        match run Expressions.plist "[  ]" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.Array() =
        let expected = Expression.Array([Expression.Variable('x');Expression.Variable('y')])

        match run Expressions.parray "[|x; y|]" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.EmptyArray() =
        let expected = Expression.Array([])

        match run Expressions.parray "[||]" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.EmptyArrayWithWhitespace() =
        let expected = Expression.Array([])

        match run Expressions.parray "[|  |]" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.TupleHint() =
        let expected =
            { MatchedNode = Expression.Tuple([Expression.Variable('x');Expression.Variable('y')]) |> HintExpr
              Suggestion = Suggestion.Expr(Expression.Identifier(["id"])) }

        match run phint "(x, y) ===> id" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.TupleInFunctionApplication() =
        let expected =
            { MatchedNode =
                Expression.FunctionApplication(
                    [ Expression.Identifier(["fst"])
                      Expression.Tuple([Expression.Variable('x');Expression.Variable('y')]) ]) |> HintExpr
              Suggestion = Suggestion.Expr(Expression.Variable('x')) }

        match run phint "fst (x, y) ===> x" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ListHint() =
        let expected =
            { MatchedNode = Expression.InfixOperator(Expression.Identifier(["::"]),
                                               Expression.Variable('x'),
                                               Expression.List([])) |> HintExpr
              Suggestion = Suggestion.Expr(Expression.List([Expression.Variable('x')])) }

        match run phint "x::[] ===> [x]" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.ArrayHint() =
        let expected =
            { MatchedNode = Expression.Array([Expression.Variable('x')]) |> HintExpr
              Suggestion = Suggestion.Expr(Expression.Variable('x')) }

        match run phint "[|x|] ===> x" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.IfStatementHint() =
        let expected =
            { MatchedNode =
                Expression.If(
                    Expression.Variable('x'),
                    Expression.Constant(Constant.Bool(true)),
                    Some(Expression.Else(Expression.Constant(Constant.Bool(false))))) |> HintExpr
              Suggestion = Suggestion.Expr(Expression.Variable('x')) }

        match run phint "if x then true else false ===> x" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.MultipleFunctionApplicationsHint() =
        let expected =
            { MatchedNode =
                Expression.FunctionApplication(
                    [ Expression.Identifier(["List";"head"])
                      Expression.Parentheses(
                          Expression.FunctionApplication(
                              [ Expression.Identifier(["List";"sort"])
                                Expression.Variable('x') ])) ]) |> HintExpr
              Suggestion = Suggestion.Expr(
                                Expression.FunctionApplication(
                                    [ Expression.Identifier(["List";"min"])
                                      Expression.Variable('x') ])) }

        match run phint "List.head (List.sort x) ===> List.min x" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Suggestion with literal string message parsed correctly.``() =
        let expected =
            { MatchedNode = Expression.Constant(Constant.Unit) |> HintExpr
              Suggestion = Suggestion.Message("Message") }

        match run phint "() ===> m\"Message\"" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Suggestion with verbatim literal string message parsed correctly.``() =
        let expected =
            { MatchedNode = Expression.Constant(Constant.Unit) |> HintExpr
              Suggestion = Suggestion.Message("Message") }

        match run phint "() ===> m@\"Message\"" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Suggestion with triple quoted string message parsed correctly.``() =
        let expected =
            { MatchedNode = Expression.Constant(Constant.Unit) |> HintExpr
              Suggestion = Suggestion.Message("Message") }

        match run phint "() ===> m\"\"\"Message\"\"\"" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Parses null inside expression as expected.``() =
        let expected =
            { MatchedNode = Expression.Null |> HintExpr
              Suggestion = Suggestion.Message("Message") }

        match run phint "null ===> m\"\"\"Message\"\"\"" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Parses null inside pattern as expected.``() =
        let expected =
            { MatchedNode = Pattern.Null |> HintPat
              Suggestion = Suggestion.Message("Message") }

        match run phint "pattern: null ===> m\"\"\"Message\"\"\"" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Parses boolean constant inside pattern as expected.``() =
        let expected =
            { MatchedNode = Pattern.Constant(Constant.Bool(true)) |> HintPat
              Suggestion = Suggestion.Message("Message") }

        match run phint "pattern: true ===> m\"\"\"Message\"\"\"" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Parses cons inside pattern as expected.``() =
        let expected =
            { MatchedNode = Pattern.Cons(Pattern.Constant(Constant.Bool(true)), Pattern.List([])) |> HintPat
              Suggestion = Suggestion.Message("Message") }

        match run phint "pattern: true::[] ===> m\"\"\"Message\"\"\"" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)

    [<Test>]
    member _.``Parses or inside pattern as expected.``() =
        let expected =
            { MatchedNode = Pattern.Or(Pattern.Constant(Constant.Bool(true)), Pattern.Constant(Constant.Bool(false))) |> HintPat
              Suggestion = Suggestion.Message("Message") }

        match run phint "pattern: true | false ===> m\"\"\"Message\"\"\"" with
        | Success(hint, _, _) -> Assert.AreEqual(expected, hint)
        | Failure(message, _, _) -> Assert.Fail(message)