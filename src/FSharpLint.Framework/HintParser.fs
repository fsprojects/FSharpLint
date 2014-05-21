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
        | Byte of byte
        | Bytes of byte[]
        | Char of char
        | Decimal of decimal
        | Double of double
        | Int16 of int16
        | Int32 of int32
        | Int64 of int64
        | IntPtr of nativeint
        | SByte of sbyte
        | Single of single
        | UInt16 of uint16
        | UInt32 of uint32
        | UInt64 of uint64
        | UIntPtr of unativeint
        | UserNum of bigint * char
        | String of string
        | Unit
        | Bool of bool
        
    [<RequireQualifiedAccess>]
    type Argument = 
        | Wildcard
        | Variable of char

    type Lambda<'t> =
        {
            Arguments: Argument list
            Body: 't
        }

    [<RequireQualifiedAccess>]
    type Expression =
        | InfixOperator of string * Expression * Expression
        | PrefixOperator of string * Expression
        | FunctionApplication of Expression list
        | Wildcard
        | Variable of char
        | Identifier of string list
        | Constant of Constant
        | Parentheses of Expression
        | Lambda of Lambda<Expression>

    type Hint =
        {
            Match: Expression
            Suggestion: Expression
        }

    let charListToString charList =
        Seq.fold (fun x y -> x + y.ToString()) "" charList

    let pischar chars =
        satisfy (fun x -> List.exists ((=) x) chars)

    let pnotchar chars =
        satisfy (fun x -> not <| List.exists ((=) x) chars)

    module Operators =
        let pfirstopchar =
            pischar ['!';'%';'&';'*';'+';'-';'.';'/';'<';'=';'>';'@';'^';'|';'~']

        let opchars =
                [
                    '>';'<';'+';'-';'*';'=';'~';'%';'&';'|';'@'
                    '#';'^';'!';'?';'/';'.';':';',';//'(';')';'[';']'
                ]

        let poperator =
            pfirstopchar .>>. many (pischar opchars)
                |>> fun (x, rest) -> x::rest

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
                |>> fun (start, rest) -> start::rest

        let private pident = 
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

        let private plongident = 
            choice
                [
                    attempt (sepBy1 pident (skipChar '.'))
                    pident |>> fun x -> [x]
                ]

        let private pidentorop =
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
                    attempt pident 
                        .>>. many (attempt (skipChar '.' >>. pident))
                        .>>. opt (skipChar '.' >>. pidentorop)
                        |>> fun ((startIdent, idents), operator) -> 
                                let identifiers = startIdent::idents
                                match operator with
                                    | Some(operator) ->
                                        identifiers@[operator]
                                    | None -> identifiers
                    attempt (pidentorop |>> fun x -> [x])
                    plongident
                ]
                    |>> List.map charListToString

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
                |>> fun x -> Map.find x escapeMap

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

        let private pstringelem, private pstringelemImpl = createParserForwardedToRef()
        do pstringelemImpl :=
            choice
                [
                    attempt pstringchar
                    skipChar '\\' >>. pnewline >>. many spaces >>. pstringelem
                ]

        let pcharacter =
            skipChar '\'' >>. pcharchar .>> pchar '\''
                |>> Char

        let pliteralstring =
            skipChar '"' >>. many pstringchar .>> skipChar '"'
                |>> fun x -> String(charListToString x)

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
                |>> fun x -> String(charListToString x)

        let private psimplechar =
            pnotchar ['\n';'\t';'\r';'\b';'\'';'\\';'"']

        let private psimpleorescapechar =
            pescapechar <|> psimplechar

        let pbytechar =
            skipChar '\'' >>. psimpleorescapechar .>> skipString "'B"
                |>> fun x -> Byte(byte x)

        let pbytearray = 
            skipChar '"' >>. many pstringchar .>> skipString "\"B"
                |>> fun x -> Bytes(System.Text.Encoding.Default.GetBytes(charListToString x))

        let pverbatimbytearray = 
            skipString "@\"" >>. many pverbatimstringchar .>> skipString "\"B"
                |>> fun x -> Bytes(System.Text.Encoding.Default.GetBytes(charListToString x))

        let ptriplequotedstring =
            skipString "\"\"\"" >>. many psimpleorescapechar .>> skipString "\"\"\""
                |>> fun x -> String(charListToString x)

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
                |>> fun x -> '0'::'x'::x

        let private poctalint =
            skipChar '0'
                >>. (skipChar 'o' <|> skipChar 'O')
                >>. many1 octal
                |>> fun x -> '0'::'o'::x

        let private pbinaryint =
            skipChar '0'
                >>. (skipChar 'b' <|> skipChar 'B')
                >>. many1 (pchar '0' <|> pchar '1')
                |>> fun x -> '0'::'b'::x

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
                |>> fun x -> SByte(sbyte(minusString x))

        let pbyte = 
            pint .>> skipString "uy"
                |>> fun x -> Byte(byte(charListToString x))

        let pint16 = 
            (opt pminus) .>>. pint .>> skipChar 's'
                |>> fun x -> Int16(int16(minusString x))

        let puint16 = 
            pint .>> skipString "us"
                |>> fun x -> UInt16(uint16(charListToString x))

        let puint32 = 
            pint .>> (skipString "u" <|> skipString "ul")
                |>> fun x -> UInt32(uint32(charListToString x))

        let pnativeint = 
            (opt pminus) .>>. pint .>> skipChar 'n'
                |>> fun x -> IntPtr(nativeint(int64(minusString x)))

        let punativeint = 
            pint .>> pstring "un"
                |>> fun x -> UIntPtr(unativeint(uint64(charListToString x)))

        let pint64 = 
            (opt pminus) .>>. pint .>> skipChar 'L'
                |>> fun x -> Int64(int64(minusString x))

        let puint64 = 
            pint .>> (skipString "UL" <|> skipString "uL")
                >>= fun x -> preturn (UInt64(uint64(charListToString x)))

        let psingle =
            (opt pminus) .>>. 
                pfloat .>> (skipChar 'F' <|> skipChar 'f')
                    |>> fun (minus, x) -> Single(if minus.IsSome then -float32(x) else float32(x))

        let private numberFormat = 
            NumberLiteralOptions.AllowMinusSign
            ||| NumberLiteralOptions.AllowFraction
            ||| NumberLiteralOptions.AllowExponent

        let private pnumber =
            numberLiteral numberFormat "number"
            |>> fun nl ->
                    if nl.IsInteger then Int32(int32 nl.String)
                    else Double(double nl.String)

        let pint32 = pnumber .>> optional (skipChar 'l')

        let pdouble = pnumber

        let pbignum =
            (opt pminus) .>>. pint .>>. anyOf ['Q'; 'R'; 'Z'; 'I'; 'N'; 'G']
                |>> fun (x, t) -> UserNum(bigint.Parse(minusString x), t)

        let pdecimal =
            let pdecimalint =
                (opt pminus) .>>. pint .>> (skipChar 'M' <|> skipChar 'm')
                    |>> fun x -> Decimal(decimal (minusString x))

            let pdecimalfloat =
                (opt pminus) .>>. pfloat .>> (skipChar 'M' <|> skipChar 'm')
                    |>> fun (minus, x) -> Decimal(decimal (if minus.IsSome then -x else x))

            choice 
                [
                    attempt pdecimalint
                    pdecimalfloat
                ]

    module Constants =
        let private pbool = 
            choice
                [
                    skipString "true" >>% Bool(true)
                    skipString "false" >>% Bool(false)
                ]

        let private punit = 
            skipString "(" 
                >>. (spaces >>. skipString ")") <|> skipString ")"
                >>% Unit

        let pconstant = 
            choice
                [
                    attempt pbool
                    attempt punit
                    attempt StringAndCharacterLiterals.pcharacter
                    attempt StringAndCharacterLiterals.pliteralstring
                    attempt StringAndCharacterLiterals.pverbatimstring
                    attempt StringAndCharacterLiterals.pbytechar
                    attempt StringAndCharacterLiterals.pbytearray
                    attempt StringAndCharacterLiterals.pverbatimbytearray
                    attempt StringAndCharacterLiterals.ptriplequotedstring
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
                    NumericLiterals.pint32
                ] |>> Expression.Constant

    module Expressions =

        let pwildcard = skipString "_" >>% Expression.Wildcard

        let pargumentwildcard = skipString "_" >>% Argument.Wildcard

        let pvariable = 
            satisfy isLetter 
                .>> notFollowedBy (satisfy isLetter)
                |>> Expression.Variable

        let pargumentvariable = satisfy isLetter |>> Argument.Variable

        let plambdaarguments = sepEndBy1 (pargumentvariable <|> pargumentwildcard) spaces1

        let pexpression, private pexpressionImpl = createParserForwardedToRef()

        let pparentheses = skipChar '(' >>. pexpression .>> skipChar ')' |>> Expression.Parentheses
    
        let private plambdastart = 
            skipString "fun"
                >>. spaces1
                >>. plambdaarguments

        let private plambdaend =
            skipString "->" 
                >>. spaces
                >>. pexpression

        let plambda = 
            let plambdastart = 
                skipString "fun"
                    >>. spaces1
                    >>. plambdaarguments

            let plambdaend =
                skipString "->" 
                    >>. spaces
                    >>. pexpression

            parse {
                let! arguments = plambdastart

                let! body = plambdaend

                return Expression.Lambda({ Arguments = arguments; Body = body })
            }

        let papplication =
            choice 
                [
                    attempt Constants.pconstant
                    attempt pvariable
                    pwildcard
                    Identifiers.plongidentorop |>> Expression.Identifier
                    pparentheses
                ]

        let pfunctionapplication =
            Identifiers.plongidentorop 
                |>> Expression.Identifier
                .>> spaces
                .>>. sepEndBy1 papplication spaces
                |>> fun (func, rest) -> Expression.FunctionApplication(func::rest)

        let opp = OperatorPrecedenceParser<Expression, string, unit>()

        opp.TermParser <- 
            spaces >>.
            choice 
                [
                    attempt Constants.pconstant
                    plambda
                    attempt pvariable
                    pwildcard
                    attempt pfunctionapplication
                    Identifiers.plongidentorop |>> Expression.Identifier
                    pparentheses
                    opp.ExpressionParser
                ] .>> spaces

        // a helper function for adding infix operators to opp
        let addInfixOperator prefix precedence associativity =
            let remainingOpChars_ws = 
                if prefix = "=" then
                    notFollowedBy (pstring "==>") |>> fun _ -> ""
                else
                    manySatisfy (isAnyOf Operators.opchars)

            let op = InfixOperator(prefix, remainingOpChars_ws,
                                   precedence, associativity, (),
                                   fun remOpChars expr1 expr2 ->
                                        Expression.InfixOperator(prefix + remOpChars, expr1, expr2))
            opp.AddOperator(op)

        let addPrefixOperator op precedence =
            opp.AddOperator(PrefixOperator(op, preturn "", precedence, true, 
                                                fun expr ->
                                                    Expression.PrefixOperator(op, expr)))

        do
            addInfixOperator ";"  1 Associativity.Right

            addInfixOperator "->"  2 Associativity.Right

            addInfixOperator ":="  3 Associativity.Right

            addInfixOperator ","  4 Associativity.None

            addInfixOperator "or"  5 Associativity.Left
            addInfixOperator "||"  5 Associativity.Left

            addInfixOperator "&"  6 Associativity.Left
            addInfixOperator "&&"  6 Associativity.Left

            addInfixOperator "<"  7 Associativity.Left
            addInfixOperator ">"  7 Associativity.Left
            addInfixOperator "="  7 Associativity.Left
            addInfixOperator "|"  7 Associativity.Left
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

            addInfixOperator ":?>"  11 Associativity.None
            addInfixOperator ":?"  11 Associativity.None

            addInfixOperator "-"  12 Associativity.Left
            addInfixOperator "+"  12 Associativity.Left

            addInfixOperator "*"  13 Associativity.Left
            addInfixOperator "/"  13 Associativity.Left
            addInfixOperator "%"  13 Associativity.Left

            addInfixOperator "**" 14 Associativity.Right
            
            addPrefixOperator "+" 15
            addPrefixOperator "-" 15
            addPrefixOperator "%" 15
            addPrefixOperator "%%" 15
            addPrefixOperator "&" 15
            addPrefixOperator "&&" 15
            addPrefixOperator "!" 15
            addPrefixOperator "~" 15

            pexpressionImpl := opp.ExpressionParser

    let phint: Parser<Hint, unit> = 
        let phintcenter = 
            spaces 
                .>> skipString "===>"
                .>> spaces

        parse {
            let! m = Expressions.pexpression 

            do! phintcenter

            let! s = Expressions.pexpression

            return { Match = m; Suggestion = s }
        }