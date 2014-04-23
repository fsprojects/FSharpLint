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

    module Identifiers =
        ()

    module StringAndCharacterLiterals =
        ()

    module Operators =
        ()

    /// Not supporting hex single and hex float right now.
    /// Decimal float currently will lose precision.
    module NumericLiterals =
        let private pminus = pchar '-'

        let private minusString (minus:char option, charList) =
            if minus.IsSome then '-' :: charList else charList
                |> charListToString

        let private phexint =
            pchar '0' 
                >>. (pchar 'x' <|> pchar 'X')
                >>. many1 hex
                >>= fun x -> preturn ('0'::'x'::x)

        let private poctalint =
            pchar '0'
                >>. (pchar 'o' <|> pchar 'O')
                >>. many1 octal
                >>= fun x -> preturn ('0'::'o'::x)

        let private pbinaryint =
            pchar '0'
                >>. (pchar 'b' <|> pchar 'B')
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
            (opt pminus) .>>. pint .>> pchar 'y'
                >>= fun x -> preturn (sbyte(minusString x))

        let pbyte = 
            pint .>> pstring "uy"
                >>= fun x -> preturn (byte(charListToString x))

        let pint16 = 
            (opt pminus) .>>. pint .>> pchar 's'
                >>= fun x -> preturn (int16(minusString x))

        let puint16 = 
            pint .>> pstring "us"
                >>= fun x -> preturn (uint16(charListToString x))

        let pint32 = 
            (opt pminus) .>>. pint .>> optional (pchar 'l')
                >>= fun x -> preturn (int32(minusString x))

        let puint32 = 
            pint .>> (pstring "u" <|> pstring "ul")
                >>= fun x -> preturn (uint32(charListToString x))

        let pnativeint = 
            (opt pminus) .>>. pint .>> pchar 'n'
                >>= fun x -> preturn (nativeint(int64(minusString x)))

        let punativeint = 
            pint .>> pstring "un"
                >>= fun x -> preturn (unativeint(uint64(charListToString x)))

        let pint64 = 
            (opt pminus) .>>. pint .>> pchar 'L'
                >>= fun x -> preturn (int64(minusString x))

        let puint64 = 
            pint .>> (pstring "UL" <|> pstring "uL")
                >>= fun x -> preturn (uint64(charListToString x))

        let psingle =
            (opt pminus) .>>. 
                pfloat .>> (pchar 'F' <|> pchar 'f')
                    >>= fun (minus, x) -> preturn (if minus.IsSome then -float32(x) else float32(x))

        let pdouble =
            (opt pminus) .>>. 
                pfloat >>= fun (minus, x) -> preturn (if minus.IsSome then -x else x)

        let pbignum: Parser<bigint, unit> =
            (opt pminus) .>>. pint .>> anyOf ['Q'; 'R'; 'Z'; 'I'; 'N'; 'G']
                >>= fun x -> preturn (bigint.Parse(minusString x))

        let pdecimal: Parser<decimal, unit> =
            let pdecimalint =
                (opt pminus) .>>. pint .>> (pchar 'M' <|> pchar 'm')
                    >>= fun x -> preturn (decimal (minusString x))

            let pdecimalfloat =
                (opt pminus) .>>. pfloat .>> (pchar 'M' <|> pchar 'm')
                    >>= fun (minus, x) -> preturn (decimal (if minus.IsSome then -x else x))

            choice 
                [
                    attempt pdecimalint
                    pdecimalfloat
                ]




    let pwildcard = pstring "_" >>% Wildcard

    let pargumentwildcard = pstring "_" >>% Argument.Wildcard

    let pidentifier = 
        sepBy1 (identifier (IdentifierOptions())) (pstring ".")
            >>= fun x -> preturn (Item.Identifier(x))

    let pbool = (pstring "true" >>% Bool(true)) <|> (pstring "false" >>% Bool(false))

    let punit = 
        pstring "(" >>. ((spaces >>. pstring ")") <|> pstring ")") >>% Unit

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

    let pexpression, pexpressionImpl = createParserForwardedToRef()

    let pparentheses = pstring "(" >>. pexpression .>> pstring ")" >>= fun x -> preturn (Parentheses(x))
    
    let plambda = parse {
            let! arguments =
                pstring "fun"
                    >>. spaces1
                    >>. plambdaarguments

            let! body = 
                pstring "->" 
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
                .>> pstring "===>"
                .>> spaces

            let! s = pexpression

            return { Match = m; Suggestion = s }
        }