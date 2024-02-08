namespace FSharpLint.Framework

open System
open FParsec
open FSharp.Compiler.Tokenization
open HintParserTypes

module HintParser =

    let charListToString charList =
        Seq.fold (fun concatenatedString charElement -> concatenatedString + charElement.ToString()) String.Empty charList

    let pischar chars : Parser<char, 'CharParser> =
        satisfy (fun character -> List.exists ((=) character) chars)

    let pnotchar chars : Parser<char, 'CharParser> =
        satisfy (fun character -> not <| List.exists ((=) character) chars)

    module Operators =
        let pfirstopchar: Parser<char, unit> =
            pischar ['!';'%';'&';'*';'+';'-';'.';'/';'<';'=';'>';'@';'^';'|';'~']

        let opchars =
            [ '>';'<';'+';'-';'*';'=';'~';'%';'&';'|';'@'
              '#';'^';'!';'?';'/';'.';':';',' ]

        let poperator: Parser<char list, unit> =
            pfirstopchar
            .>>. many (pischar opchars)
            |>> fun (x, rest) -> x::rest

    /// Need to change isLetter so that it's using unicode character classes.
    module Identifiers =
        let private pidentstartchar: Parser<char, unit> =
            pchar '_' <|> satisfy isLetter

        let private pidentchar: Parser<char, unit> =
            choice
                [ satisfy isLetter
                  satisfy isDigit
                  pchar '\''
                  pchar '_' ]

        let private pidenttext: Parser<char list, unit> =
            pidentstartchar .>>. many pidentchar
            |>> fun (start, rest) -> start::rest
            >>= fun ident ->
                let identStr = System.String.Join(String.Empty, ident)

                let isKeyword = List.exists ((=) identStr) FSharpKeywords.KeywordNames

                if isKeyword then fail (sprintf "Unexpected keyword %s" identStr)
                else preturn ident

        let private pident: (CharStream<unit> -> Reply<char list>) =
            let chars = ['`'; '\n'; '\r'; '\t']

            choice
                [ pidenttext
                  skipString "``"
                  >>. many1
                          (choice
                              [ attempt (pnotchar chars)
                                attempt (pchar '`' >>. pnotchar chars) ])
                  .>> skipString "``" ]

        let private plongident: (CharStream<unit> -> Reply<char list list>) =
            choice
                [ attempt (sepBy1 pident (skipChar '.'))
                  pident |>> fun identChars -> [identChars] ]

        let private pidentorop: (CharStream<unit> -> Reply<char list>) =
            choice
                [ attempt pident
                  skipChar '('
                  .>> spaces
                  >>. Operators.poperator
                  .>> spaces
                  .>> skipChar ')' ]

        let plongidentorop: Parser<string list, unit> =
            choice
                [ attempt pident
                  .>>. many (attempt (skipChar '.' >>. pident))
                  .>>. opt (skipChar '.' >>. pidentorop)
                  |>> (fun ((startIdent, idents), operator) ->
                      let identifiers = startIdent::idents
                      match operator with
                      | Some(operator) -> identifiers@[operator]
                      | None -> identifiers)
                  attempt (pidentorop |>> fun identOrOpChars -> [identOrOpChars])
                  plongident ]
            |>> List.map charListToString

    module StringAndCharacterLiterals =
        let private hexToCharacter hex =
            char(System.Convert.ToInt32(hex, 16))

        let private decimalToCharacter dec =
            char(System.Convert.ToInt32(dec, 10))

        let private escapeMap =
            Map.ofList
                [
                    ('"', '\"')
                    ('\\', '\\')
                    ('\'', '\'')
                    ('n', '\n')
                    ('t', '\t')
                    ('b', '\b')
                    ('r', '\r')
                    ('a', '\a')
                    ('f', '\f')
                    ('v', '\v')
                ]

        let private pescapechar: Parser<char, unit> =
            skipChar '\\'
            >>. pischar ['"';'\\';'\'';'n';'t';'b';'r';'a';'f';'v']
            |>> fun escapeChar ->
                match (Map.tryFind escapeChar escapeMap) with
                | Some value -> value
                | None -> failwith "Invalid escape character."

        let private pnonescapechars: Parser<char, unit> =
            skipChar '\\'
            >>. pnotchar ['"';'\\';'\'';'n';'t';'b';'r';'a';'f';'v']

        let private psimplecharchar: Parser<char, unit> =
            pnotchar ['\n';'\t';'\r';'\b';'\a';'\f';'\v';'\\';'\'']

        let private psimplestringchar: Parser<char, unit> =
            pnotchar ['"';'\n';'\t';'\r';'\b';'\a';'\f';'\v';'\\']

        let private punicodegraphshort: Parser<char, unit> =
            skipString "\\u"
            >>. many1 hex
            >>= fun unicodegraphChars ->
                if unicodegraphChars.Length <> 4 then
                    fail "Unicode graph short must be 4 hex characters long"
                else
                    preturn (unicodegraphChars |> charListToString |> hexToCharacter)

        let private punicodegraphlong: Parser<char, unit> =
            skipString "\\U"
            >>. many1 hex
            >>= fun unicodegraphChars ->
                if unicodegraphChars.Length <> 8 then
                    fail "Unicode graph long must be 8 hex characters long"
                else
                    preturn (unicodegraphChars |> charListToString |> hexToCharacter)

        let private ptrigraph: Parser<char, unit> =
            skipChar '\\'
            >>. many1 digit
            >>= fun trigraphChars ->
                if trigraphChars.Length <> 3 then
                    fail "Trigraph must be 3 characters long"
                else
                    preturn (trigraphChars |> charListToString |> decimalToCharacter)

        let private pnewline: Parser<char, unit> =
            pchar '\n' <|> (skipChar '\r' >>. skipChar '\n' >>% '\n')

        let private pcharchar: (CharStream<unit> -> Reply<char>) =
            choice
                [ attempt psimplecharchar
                  attempt pescapechar
                  attempt ptrigraph
                  punicodegraphshort ]

        let private pstringchar: (CharStream<unit> -> Reply<char>) =
            choice
                [ attempt psimplestringchar
                  attempt ptrigraph
                  attempt punicodegraphlong
                  attempt punicodegraphshort
                  attempt pescapechar
                  attempt pnonescapechars
                  pnewline ]

        let private pstringelem, private pstringelemImpl = createParserForwardedToRef()

        do pstringelemImpl :=
            choice
                [ attempt pstringchar
                  skipChar '\\' >>. pnewline >>. many spaces >>. pstringelem ]

        let pcharacter: Parser<Constant, unit> =
            skipChar '\''
            >>. pcharchar
            .>> pchar '\''
            |>> Char

        let pliteralstring: Parser<string, unit> =
            skipChar '"'
            >>. many pstringchar
            .>> skipChar '"'
            |>> charListToString

        let private pverbatimstringchar: (CharStream<unit> -> Reply<char>) =
            choice
                [ pstringelem
                  pnonescapechars
                  pnewline
                  pchar '\\'
                  pstring "\"\"" >>% '"' ]

        let pverbatimstring: Parser<string, unit> =
            pstring "@\""
            >>. many pverbatimstringchar
            .>> pchar '"'
            |>> charListToString

        let private psimplechar: Parser<char, unit> =
            pnotchar ['\n';'\t';'\r';'\b';'\'';'\\';'"']

        let private psimpleorescapechar: Parser<char, unit> =
            pescapechar <|> psimplechar

        let pbytechar: Parser<Constant, unit> =
            skipChar '\''
            >>. psimpleorescapechar
            .>> skipString "'B"
            |>> (byte >> Byte)

        let pbytearray: Parser<Constant, unit> =
            skipChar '"'
            >>. many pstringchar
            .>> skipString "\"B"
            |>> (charListToString >> System.Text.Encoding.UTF8.GetBytes >> Bytes)

        let pverbatimbytearray: Parser<Constant, unit> =
            skipString "@\""
            >>. many pverbatimstringchar
            .>> skipString "\"B"
            |>> (charListToString >> System.Text.Encoding.UTF8.GetBytes >> Bytes)

        let ptriplequotedstring: Parser<string, unit> =
            skipString "\"\"\""
            >>. many psimpleorescapechar
            .>> skipString "\"\"\""
            |>> charListToString

    /// Not supporting hex single and hex float right now.
    /// Decimal float currently will lose precision.
    module NumericLiterals =
        let private pminus: Parser<char, unit> = pchar '-'

        let private minusString (minus:char option, charList) =
            if minus.IsSome then '-' :: charList else charList
            |> charListToString

        let private phexint: Parser<char list, unit> =
            skipChar '0'
            >>. (skipChar 'x' <|> skipChar 'X')
            >>. many1 hex
            |>> fun hexChars -> '0'::'x'::hexChars

        let private poctalint: Parser<char list, unit> =
            skipChar '0'
            >>. (skipChar 'o' <|> skipChar 'O')
            >>. many1 octal
            |>> fun octalChars -> '0'::'o'::octalChars

        let private pbinaryint: Parser<char list, unit> =
            skipChar '0'
            >>. (skipChar 'b' <|> skipChar 'B')
            >>. many1 (pchar '0' <|> pchar '1')
            |>> fun binaryChars -> '0'::'b'::binaryChars

        let private pint: (CharStream<unit> -> Reply<char list>) =
            choice
                [ attempt phexint
                  attempt poctalint
                  attempt pbinaryint
                  many1 digit ]

        let psbyte: Parser<Constant, unit> =
            opt pminus
            .>>. pint
            .>> skipChar 'y'
            |>> (minusString >> sbyte >> SByte)

        let pbyte: Parser<Constant, unit> =
            pint
            .>> skipString "uy"
            |>> (charListToString >> byte >> Byte)

        let pint16: Parser<Constant, unit> =
            opt pminus
            .>>. pint .>> skipChar 's'
            |>> (minusString >> int16 >> Int16)

        let puint16: Parser<Constant, unit> =
            pint
            .>> skipString "us"
            |>> (charListToString >> uint16 >> UInt16)

        let puint32: Parser<Constant, unit> =
            pint
            .>> (skipString "u" <|> skipString "ul")
            |>> (charListToString >> uint32 >> UInt32)

        let pnativeint: Parser<Constant, unit> =
            opt pminus
            .>>. pint .>> skipChar 'n'
            |>> (minusString >> int64 >> nativeint >> IntPtr)

        let punativeint: Parser<Constant, unit> =
            pint
            .>> pstring "un"
            |>> (charListToString >> uint64 >> unativeint >> UIntPtr)

        let pint64: Parser<Constant, unit> =
            opt pminus
            .>>. pint
            .>> skipChar 'L'
            |>> (minusString >> int64 >> Int64)

        let puint64: Parser<Constant, unit> =
            pint
            .>> (skipString "UL" <|> skipString "uL")
            >>= (charListToString >> uint64 >> UInt64 >> preturn)

        let psingle: Parser<Constant, unit> =
            opt pminus
            .>>. pfloat
            .>> (skipChar 'F' <|> skipChar 'f')
            |>> fun (minus, x) -> Single(if minus.IsSome then -float32(x) else float32(x))

        let private numberFormat =
            NumberLiteralOptions.AllowMinusSign
            ||| NumberLiteralOptions.AllowFraction
            ||| NumberLiteralOptions.AllowExponent

        let private pnumber: Parser<Constant, unit> =
            numberLiteral numberFormat "number"
            |>> fun nl ->
                if nl.IsInteger then Int32(int32 nl.String)
                else Double(double nl.String)

        let pint32: Parser<Constant, unit> = pnumber .>> optional (skipChar 'l')

        let pdouble: Parser<Constant, unit> = pnumber

        let pbignum: Parser<Constant, unit> =
            opt pminus
            .>>. pint
            .>>. anyOf ['Q'; 'R'; 'Z'; 'I'; 'N'; 'G']
            |>> fun (x, t) -> UserNum(bigint.Parse(minusString x), t)

        let pdecimal: Parser<Constant, unit> =
            let pdecimalint: Parser<Constant, unit> =
                opt pminus
                .>>. pint
                .>> (skipChar 'M' <|> skipChar 'm')
                |>> (minusString >> decimal >> Decimal)

            let pdecimalfloat: Parser<Constant, unit> =
                opt pminus
                .>>. pfloat
                .>> (skipChar 'M' <|> skipChar 'm')
                |>> fun (minus, x) -> Decimal(decimal (if minus.IsSome then -x else x))

            choice
                [ attempt pdecimalint
                  pdecimalfloat ]

    module Constants =
        let private pbool: (CharStream<unit> -> Reply<Constant>) =
            choice
                [ skipString "true" >>% Bool(true)
                  skipString "false" >>% Bool(false) ]

        let private punit: Parser<Constant, unit> =
            skipString "("
            >>. ((spaces >>. skipString ")") <|> skipString ")")
            >>% Unit

        let pconstant: Parser<Constant, unit> =
            choice
                [ attempt pbool
                  attempt punit
                  attempt StringAndCharacterLiterals.pcharacter
                  attempt StringAndCharacterLiterals.pliteralstring |>> String
                  attempt StringAndCharacterLiterals.pverbatimstring |>> String
                  attempt StringAndCharacterLiterals.pbytechar
                  attempt StringAndCharacterLiterals.pbytearray
                  attempt StringAndCharacterLiterals.pverbatimbytearray
                  attempt StringAndCharacterLiterals.ptriplequotedstring |>> String
                  attempt NumericLiterals.psbyte
                  attempt NumericLiterals.pbyte
                  attempt NumericLiterals.pint16
                  attempt NumericLiterals.puint16
                  attempt NumericLiterals.puint32
                  attempt NumericLiterals.pnativeint
                  attempt NumericLiterals.punativeint
                  attempt NumericLiterals.pint64
                  attempt NumericLiterals.puint64
                  attempt NumericLiterals.psingle
                  attempt NumericLiterals.pbignum
                  attempt NumericLiterals.pdecimal
                  attempt NumericLiterals.pdouble
                  NumericLiterals.pint32 ]

    module CommonParsers =

        let pvariable: Parser<char, unit> =
            satisfy isLetter
            .>> notFollowedBy (satisfy isLetter)

        let ptuple (pparser:Parser<'Element, unit>) : Parser<'Element list, unit> =
            skipChar '('
            >>. pparser
            .>> skipChar ','
            .>>. sepEndBy1 pparser (skipChar ',')
            .>> skipChar ')'
            |>> fun (func, rest) -> (func::rest)

        let plist (pparser:Parser<'Element, unit>): Parser<'Element list, unit> =
            skipChar '['
            >>. spaces
            >>. sepEndBy pparser (skipChar ';')
            .>> spaces
            .>> skipChar ']'

        let parray (pparser:Parser<'Element, unit>): Parser<'Element list, unit> =
            skipString "[|"
            >>. spaces
            >>. sepEndBy pparser (skipChar ';')
            .>> spaces
            .>> skipString "|]"

    module Expressions =

        let pwildcard: Parser<Expression, unit> = skipString "_" >>% Expression.Wildcard

        let pvariable: Parser<Expression, unit> = CommonParsers.pvariable |>> Expression.Variable

        let pargumentvariable: Parser<Expression, unit> = satisfy isLetter |>> Expression.Variable

        let plambdaarguments: Parser<Expression list, unit> = sepEndBy1 (pargumentvariable <|> pwildcard) spaces1

        let pexpression, private pexpressionImpl = createParserForwardedToRef()

        let pparentheses: Parser<Expression, unit> = skipChar '(' >>. pexpression .>> skipChar ')' |>> Expression.Parentheses

        let pif: Parser<Expression, unit> =
            skipString "if"
            >>. spaces
            >>. pexpression
            .>> spaces
            .>> skipString "then"
            .>>. pexpression
            .>>. opt (skipString "else" >>. pexpression)
            |>> fun ((condition, expr), elseExpr) -> Expression.If(condition, expr, Option.map Expression.Else elseExpr)

        let plambda: Parser<Expression, unit> =
            let plambdastart: Parser<Expression list, unit> =
                skipString "fun"
                >>. spaces1
                >>. plambdaarguments

            let plambdaend: Parser<Expression, unit> =
                skipString "->"
                >>. spaces
                >>. pexpression

            parse {
                let! arguments = plambdastart

                let! body = plambdaend

                return Expression.Lambda
                    (List.map (Expression.LambdaArg >> LambdaArg) arguments,
                     LambdaBody(Expression.LambdaBody(body)))
            }

        let ptuple = (CommonParsers.ptuple pexpression) |>> Expression.Tuple

        let plist = (CommonParsers.plist pexpression) |>> Expression.List

        let parray = (CommonParsers.parray pexpression) |>> Expression.Array

        let papplication =
            choice
                [ attempt Constants.pconstant |>> Expression.Constant
                  attempt pvariable
                  attempt pwildcard
                  attempt Identifiers.plongidentorop |>> Expression.Identifier
                  attempt ptuple
                  attempt plist
                  attempt parray
                  attempt pparentheses ]

        let pfunctionapplication: Parser<Expression, unit> =
            Identifiers.plongidentorop
            |>> Expression.Identifier
            .>> spaces
            .>>. sepEndBy1 papplication spaces
            |>> fun (func, rest) -> Expression.FunctionApplication(func::rest)

        let opp = OperatorPrecedenceParser<Expression, string, unit>()

        let prefixoperatorterm: Parser<Expression, unit> =
            followedBy (pischar ['+';'-';'%';'&';'!';'~']) >>. opp.ExpressionParser

        let validBangPrefixedOperatorChars = ['!'; '%'; '&'; '*'; '+'; '.'; '/'; '<'; '='; '>'; '@'; '^'; '|'; '~'; '?']

        opp.TermParser <-
            spaces >>.
            choice
                [ attempt pif
                  attempt (skipString "null" >>% Expression.Null)
                  attempt Constants.pconstant |>> Expression.Constant
                  attempt plambda
                  attempt pvariable
                  attempt pwildcard
                  attempt pfunctionapplication
                  attempt Identifiers.plongidentorop |>> Expression.Identifier
                  attempt ptuple
                  attempt plist
                  attempt parray
                  attempt pparentheses
                  prefixoperatorterm ] .>> spaces

        // a helper function for adding infix operators to opp
        let addInfixOperator prefix precedence associativity =
            let remainingOpChars =
                if prefix = "=" then
                    notFollowedBy (pstring "==>") |>> fun _ -> String.Empty
                else if prefix = "|" then
                    notFollowedBy (pstring "]") |>> fun _ -> String.Empty
                else
                    manySatisfy (isAnyOf Operators.opchars)

            let op = InfixOperator(prefix, remainingOpChars,
                                   precedence, associativity, (),
                                   fun remOpChars expr1 expr2 ->
                                        let opIdent = Expression.Identifier [prefix + remOpChars]
                                        Expression.InfixOperator(opIdent, expr1, expr2))
            opp.AddOperator(op)

        let addPrefixOperator prefix precedence =
            let remainingOpChars =
                if prefix = "!" then
                    manySatisfy (isAnyOf validBangPrefixedOperatorChars)
                else if prefix = "~" then
                    manySatisfy ((=) '~')
                else
                    preturn String.Empty

            let checkPrefix remOpChars expr =
                if prefix = "&" then Expression.AddressOf(true, expr)
                else if prefix = "&&" then Expression.AddressOf(false, expr)
                else if prefix = "!" || prefix = "~" then
                    let opIdent = Expression.Identifier [prefix + remOpChars]
                    Expression.PrefixOperator(opIdent, expr)
                else
                    let opIdent = Expression.Identifier ["~" + prefix + remOpChars]
                    Expression.PrefixOperator(opIdent, expr)

            let prefixOp =
                PrefixOperator(
                    prefix, remainingOpChars, precedence, true, (),
                    checkPrefix)

            opp.AddOperator(prefixOp)

        do
            addInfixOperator ":="  3 Associativity.Right

            addInfixOperator "or"  4 Associativity.Left
            addInfixOperator "||"  4 Associativity.Left

            addInfixOperator "|"  5 Associativity.Left

            addInfixOperator "&"  6 Associativity.Left
            addInfixOperator "&&"  6 Associativity.Left

            addInfixOperator "<"  7 Associativity.Left
            addInfixOperator ">"  7 Associativity.Left
            addInfixOperator "="  7 Associativity.Left
            for i in Operators.opchars do
                if i <> '&' then
                    addInfixOperator ("&" + i.ToString())  7 Associativity.Left

            addInfixOperator "&&&"  8 Associativity.Left
            addInfixOperator "|||"  8 Associativity.Left
            addInfixOperator "^^^"  8 Associativity.Left
            addInfixOperator "~~~"  8 Associativity.Left
            addInfixOperator "<<<"  8 Associativity.Left
            addInfixOperator ">>>"  8 Associativity.Left

            addInfixOperator "^"  9 Associativity.Right

            addInfixOperator "::"  10 Associativity.Right
            addInfixOperator "@"  10 Associativity.Right

            addInfixOperator ":?>"  11 Associativity.None
            addInfixOperator ":?"  11 Associativity.None

            addInfixOperator "-"  12 Associativity.Left
            addInfixOperator "+"  12 Associativity.Left

            addInfixOperator "*"  13 Associativity.Left
            addInfixOperator "/"  13 Associativity.Left
            addInfixOperator "%"  13 Associativity.Left

            addInfixOperator "**" 14 Associativity.Right

            addPrefixOperator "~" 15
            addPrefixOperator "+" 15
            addPrefixOperator "+." 15
            addPrefixOperator "-" 15
            addPrefixOperator "-." 15
            addPrefixOperator "%" 15
            addPrefixOperator "%%" 15
            addPrefixOperator "&" 15
            addPrefixOperator "&&" 15

            addPrefixOperator "!" 15

            pexpressionImpl := opp.ExpressionParser

    module Patterns =

        open CommonParsers

        let ppattern, private ppatternImpl = createParserForwardedToRef()

        let pwildcard: Parser<Pattern, unit> = skipString "_" >>% Pattern.Wildcard

        let pparentheses: Parser<Pattern, unit> = skipChar '(' >>. ppattern .>> skipChar ')' |>> Pattern.Parentheses

        let opp = OperatorPrecedenceParser<Pattern, string, unit>()

        opp.TermParser <-
            spaces >>.
            choice
                [ (skipString "null" >>% Pattern.Null)
                  Constants.pconstant |>> Pattern.Constant
                  pvariable |>> Pattern.Variable
                  pwildcard
                  Identifiers.plongidentorop |>> Pattern.Identifier
                  (ptuple ppattern) |>> Pattern.Tuple
                  (parray ppattern) |>> Pattern.Array
                  (plist ppattern) |>> Pattern.List
                  pparentheses ] .>> spaces

        // a helper function for adding infix operators to opp
        let addInfixOperator operator precedence associativity =
            let remainingOpChars =
                if operator = "|" then
                    notFollowedBy (pstring "]") |>> fun _ -> String.Empty
                else
                    manySatisfy (isAnyOf Operators.opchars)

            let op = InfixOperator(operator, remainingOpChars,
                                   precedence, associativity, (),
                                   fun _ patLhs patRhs ->
                                        match operator with
                                        | "|" -> Pattern.Or(patLhs, patRhs)
                                        | "::" -> Pattern.Cons(patLhs, patRhs)
                                        | _ -> failwith ("Unexpected operator " + operator + " in pattern."))
            opp.AddOperator(op)

        do
            addInfixOperator "|"  1 Associativity.Left
            addInfixOperator "::"  2 Associativity.Left
            // TODO: addInfixOperator "&"  3 Associativity.Left

            ppatternImpl := opp.ExpressionParser

    let private psuggestion: Parser<Suggestion, Unit> =
        let pstring =
            choice
                [ StringAndCharacterLiterals.ptriplequotedstring
                  StringAndCharacterLiterals.pverbatimstring
                  StringAndCharacterLiterals.pliteralstring ]

        choice
            [ attempt (pchar 'm' >>. pstring |>> Suggestion.Message)
              Expressions.pexpression |>> Suggestion.Expr ]

    let private phintcenter: Parser<unit, unit> =
        spaces
        .>> skipString "===>"
        .>> spaces

    let pexpressionbasedhint =
        parse {
            let! expression = Expressions.pexpression

            do! phintcenter

            let! suggestion = psuggestion

            return { MatchedNode = HintExpr expression; Suggestion = suggestion }
        }

    let ppatternbasedhint =
        parse {
            let! pattern = Patterns.ppattern

            do! phintcenter

            let! suggestion = psuggestion

            return { MatchedNode = HintPat pattern; Suggestion = suggestion }
        }

    let phint: Parser<Hint, unit> =
        choice
            [ spaces >>. (skipString "pattern:") >>. spaces >>. ppatternbasedhint
              pexpressionbasedhint ]