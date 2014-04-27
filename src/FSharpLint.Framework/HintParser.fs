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

namespace FSharpLint.Framework

open FParsec

module HintParser =

    type Constant =
        | Float of float
        | Unit
        | Bool of bool

    type Argument = 
        | Wildcard
        | Variable of char

    type Lambda<'t> =
        {
            Arguments: Argument list
            Body: 't
        }

    type Item =
        | Wildcard
        | Operator of char list
        | Variable of char
        | Identifier of string list
        | Constant of Constant
        | Expression of Item list
        | Parentheses of Item
        | Lambda of Lambda<Item>

    type Hint =
        {
            Match: Item
            Suggestion: Item
        }

    let charListToString charList =
        Seq.fold (fun x y -> x + y.ToString()) "" charList

    let pischar chars =
        satisfy (fun x -> List.exists ((=) x) chars)

    let pnotchar chars =
        satisfy (fun x -> not <| List.exists ((=) x) chars)

    module Operators =

        let private pfirstopchar =
            pischar ['!';'%';'&';'*';'+';'-';'.';'/';'<';'=';'>';'@';'^';'|';'~']

        let private popchar =
            pischar 
                [
                    '>';'<';'+';'-';'*';'=';'~';'%';'.';'&';'|';'@'
                    '#';'^';'!';'?';'/';'.';':';',';//'(';')';'[';']'
                ]

        let poperator =
            pfirstopchar .>>. many popchar
                >>= fun (x, rest) -> preturn (x::rest)

    /// Need to change isLetter so that it's using unicode character classes.
    module Identifiers =
        let private pidentstartchar =
            pchar '_' <|> satisfy isLetter

        let private pidentchar: Parser<char, unit> = 
            choice
                [
                    satisfy isLetter
                    satisfy isDigit
                    pchar '\''
                    pchar '_'
                ]

        let private pidenttext = 
            pidentstartchar .>>. many pidentchar
                >>= fun (start, rest) -> preturn (start::rest)

        let pident = 
            let chars = ['`'; '\n'; '\r'; '\t']

            choice
                [
                    pidenttext
                    skipString "``" 
                        >>. many1
                                (choice
                                    [
                                        attempt (pnotchar chars)
                                        attempt (pchar '`' >>. pnotchar chars)
                                    ])
                        .>> skipString "``"
                ]

        let plongident = 
            choice
                [
                    attempt (sepBy pident (skipChar '.'))
                    pident >>= fun x -> preturn [x]
                ]

        let pidentorop =
            choice
                [
                    attempt pident
                    skipChar '(' 
                        .>> spaces
                        >>. Operators.poperator 
                        .>> spaces
                        .>> skipChar ')'
                ]

        let plongidentorop = 
            choice
                [
                    attempt 
                        (
                            many (pident .>> skipChar '.') .>>. pidentorop
                                >>= fun (identifiers, identifierOrOp) -> 
                                        preturn (identifiers@[identifierOrOp])
                        )
                    attempt (pidentorop >>= fun x -> preturn [x])
                    plongident
                ]

    module StringAndCharacterLiterals =
        let private hexToCharacter hex =
            char(System.Convert.ToInt32(hex, 16))

        let private decimalToCharacter dec =
            char(System.Convert.ToInt32(dec, 10))

        let private escapeMap =
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
            ] |> Map.ofList

        let private pescapechar: Parser<char, unit> = 
            skipChar '\\'
                >>. pischar ['"';'\\';'\'';'n';'t';'b';'r';'a';'f';'v']
                >>= fun x -> preturn (Map.find x escapeMap)

        let private pnonescapechars =
            skipChar '\\'
                >>. pnotchar ['"';'\\';'\'';'n';'t';'b';'r';'a';'f';'v']

        let private psimplecharchar =
            pnotchar ['\n';'\t';'\r';'\b';'\a';'\f';'\v';'\\';'\'']

        let private psimplestringchar =
            pnotchar ['"';'\n';'\t';'\r';'\b';'\a';'\f';'\v';'\\';'\'']

        let private punicodegraphshort = 
            skipString "\\u"
                >>. many1 hex
                >>= fun x ->
                    if x.Length <> 4 then
                        fail "Unicode graph short must be 4 hex characters long"
                    else
                        preturn (x |> charListToString |> hexToCharacter)

        let private punicodegraphlong =
            skipString "\\U"
                >>. many1 hex
                >>= fun x ->
                    if x.Length <> 8 then
                        fail "Unicode graph long must be 8 hex characters long"
                    else
                        preturn (x |> charListToString |> hexToCharacter)

        let private ptrigraph =
            skipChar '\\'
                >>. many1 digit
                >>= fun x ->
                    if x.Length <> 3 then
                        fail "Trigraph must be 3 characters long"
                    else
                        preturn (x |> charListToString |> decimalToCharacter)

        let private pnewline = 
            pchar '\n' <|> (skipChar '\r' >>. skipChar '\n' >>% '\n')

        let private pcharchar =
            choice 
                [
                    attempt psimplecharchar
                    attempt pescapechar
                    attempt ptrigraph
                    punicodegraphshort
                ]

        let private pstringchar =
            choice
                [
                    attempt psimplestringchar
                    attempt ptrigraph
                    attempt punicodegraphlong
                    attempt punicodegraphshort
                    attempt pescapechar
                    attempt pnonescapechars
                    pnewline
                ]

        let pstringelem, private pstringelemImpl = createParserForwardedToRef()
        do pstringelemImpl :=
            choice
                [
                    attempt pstringchar
                    skipChar '\\' >>. pnewline >>. many spaces >>. pstringelem
                ]

        let pcharacter =
            skipChar '\'' >>. pcharchar .>> pchar '\''

        let pliteralstring =
            skipChar '"' >>. many pstringchar .>> skipChar '"'
                >>= fun x -> preturn (charListToString x)

        let private pverbatimstringchar =
            choice
                [
                    pstringelem
                    pnonescapechars
                    pnewline
                    pchar '\\'
                    pstring "\"\"" >>% '"'
                ]

        let pverbatimstring =
            pstring "@\"" >>. many pverbatimstringchar .>> pchar '"'

        let private psimplechar =
            pnotchar ['\n';'\t';'\r';'\b';'\'';'\\';'"']

        let private psimpleorescapechar =
            pescapechar <|> psimplechar

        let pbytechar =
            skipChar '\'' >>. psimpleorescapechar .>> skipString "'B"
                >>= fun x -> preturn (byte x)

        let pbytearray = 
            skipChar '"' >>. many pstringchar .>> skipString "\"B"
                >>= fun x -> preturn (System.Text.Encoding.Default.GetBytes(charListToString x))

        let pverbatimbytearray = 
            skipString "@\"" >>. many pverbatimstringchar .>> skipString "\"B"

        let ptriplequotedstring =
            skipString "\"\"\"" >>. many psimpleorescapechar .>> skipString "\"\"\""

    /// Not supporting hex single and hex float right now.
    /// Decimal float currently will lose precision.
    module NumericLiterals =
        let private pminus = pchar '-'

        let private minusString (minus:char option, charList) =
            if minus.IsSome then '-' :: charList else charList
                |> charListToString

        let private phexint =
            skipChar '0' 
                >>. (skipChar 'x' <|> skipChar 'X')
                >>. many1 hex
                >>= fun x -> preturn ('0'::'x'::x)

        let private poctalint =
            skipChar '0'
                >>. (skipChar 'o' <|> skipChar 'O')
                >>. many1 octal
                >>= fun x -> preturn ('0'::'o'::x)

        let private pbinaryint =
            skipChar '0'
                >>. (skipChar 'b' <|> skipChar 'B')
                >>. many1 (pchar '0' <|> pchar '1')
                >>= fun x -> preturn ('0'::'b'::x)

        let private pint =
            choice
                [
                    attempt phexint
                    attempt poctalint
                    attempt pbinaryint
                    many1 digit
                ]

        let psbyte = 
            (opt pminus) .>>. pint .>> skipChar 'y'
                >>= fun x -> preturn (sbyte(minusString x))

        let pbyte = 
            pint .>> skipString "uy"
                >>= fun x -> preturn (byte(charListToString x))

        let pint16 = 
            (opt pminus) .>>. pint .>> skipChar 's'
                >>= fun x -> preturn (int16(minusString x))

        let puint16 = 
            pint .>> skipString "us"
                >>= fun x -> preturn (uint16(charListToString x))

        let pint32 = 
            (opt pminus) .>>. pint .>> optional (skipChar 'l')
                >>= fun x -> preturn (int32(minusString x))

        let puint32 = 
            pint .>> (skipString "u" <|> skipString "ul")
                >>= fun x -> preturn (uint32(charListToString x))

        let pnativeint = 
            (opt pminus) .>>. pint .>> skipChar 'n'
                >>= fun x -> preturn (nativeint(int64(minusString x)))

        let punativeint = 
            pint .>> pstring "un"
                >>= fun x -> preturn (unativeint(uint64(charListToString x)))

        let pint64 = 
            (opt pminus) .>>. pint .>> skipChar 'L'
                >>= fun x -> preturn (int64(minusString x))

        let puint64 = 
            pint .>> (skipString "UL" <|> skipString "uL")
                >>= fun x -> preturn (uint64(charListToString x))

        let psingle =
            (opt pminus) .>>. 
                pfloat .>> (skipChar 'F' <|> skipChar 'f')
                    >>= fun (minus, x) -> preturn (if minus.IsSome then -float32(x) else float32(x))

        let pdouble =
            (opt pminus) .>>. 
                pfloat >>= fun (minus, x) -> preturn (if minus.IsSome then -x else x)

        let pbignum: Parser<bigint, unit> =
            (opt pminus) .>>. pint .>> anyOf ['Q'; 'R'; 'Z'; 'I'; 'N'; 'G']
                >>= fun x -> preturn (bigint.Parse(minusString x))

        let pdecimal: Parser<decimal, unit> =
            let pdecimalint =
                (opt pminus) .>>. pint .>> (skipChar 'M' <|> skipChar 'm')
                    >>= fun x -> preturn (decimal (minusString x))

            let pdecimalfloat =
                (opt pminus) .>>. pfloat .>> (skipChar 'M' <|> skipChar 'm')
                    >>= fun (minus, x) -> preturn (decimal (if minus.IsSome then -x else x))

            choice 
                [
                    attempt pdecimalint
                    pdecimalfloat
                ]

    module Expressions =
        ()

    let pwildcard = skipString "_" >>% Wildcard

    let pargumentwildcard = skipString "_" >>% Argument.Wildcard

    let pidentifier = 
        sepBy1 (identifier (IdentifierOptions())) (pstring ".")
            >>= fun x -> preturn (Item.Identifier(x))

    let pbool = (skipString "true" >>% Bool(true)) <|> (skipString "false" >>% Bool(false))

    let punit = 
        skipString "(" >>. ((spaces >>. skipString ")") <|> skipString ")") >>% Unit

    let isOperator character =
        let operators =
            [
                '+'
                '*'
                '/'
                '%'
                '&'
                '|'
                '='
                '<'
                '>'
            ]

        List.exists ((=) character) operators

    let poperator: Parser<Item, unit> = 
        many1 (satisfy isOperator) 
            >>= fun x -> 
                if charListToString x = "===>" then 
                    fail "Found ===>" 
                else 
                    preturn (Item.Operator(x))

    let pconstant = 
        choice
            [
                pbool
                pfloat >>= fun x -> preturn (Float(x))
                punit
            ] >>= fun x -> preturn (Constant(x))

    let pvariable = 
        satisfy isLetter 
            .>> notFollowedBy (satisfy isLetter)
            >>= fun x -> preturn (Item.Variable(x))

    let pargumentvariable = satisfy isLetter >>= fun x -> preturn (Argument.Variable(x))

    let plambdaarguments = sepEndBy1 (pargumentvariable <|> pargumentwildcard) spaces1

    let pexpression, private pexpressionImpl = createParserForwardedToRef()

    let pparentheses = skipChar '(' >>. pexpression .>> skipChar ')' >>= fun x -> preturn (Parentheses(x))
    
    let plambda = parse {
            let! arguments =
                skipString "fun"
                    >>. spaces1
                    >>. plambdaarguments

            let! body = 
                skipString "->" 
                    >>. spaces
                    >>. pexpression

            return Lambda({ Arguments = arguments; Body = body })
        }

    do pexpressionImpl := 
            let item = 
                choice
                    [
                        attempt pconstant
                        plambda
                        attempt pvariable
                        pwildcard
                        pidentifier
                        attempt poperator
                        pparentheses
                    ]

            sepEndBy item spaces1 >>= fun x -> preturn (Expression(x))

    let phint: Parser<Hint, unit> = 
        parse {
            let! m = pexpression 

            do! spaces 
                .>> skipString "===>"
                .>> spaces

            let! s = pexpression

            return { Match = m; Suggestion = s }
        }