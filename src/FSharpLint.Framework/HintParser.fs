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
                if Seq.fold (fun x y -> x + y.ToString()) "" x = "===>" then 
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