namespace FSharpLint.Framework

open FParsec
open FSharp.Compiler.Tokenization

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
    type Pattern =
        | Cons of Pattern * Pattern
        | Or of Pattern * Pattern
        | Wildcard
        | Variable of char
        | Identifier of string list
        | Constant of Constant
        | Parentheses of Pattern
        | Tuple of Pattern list
        | List of Pattern list
        | Array of Pattern list
        | Null

    [<RequireQualifiedAccess>]
    type Expression =
        | FunctionApplication of Expression list
        | InfixOperator of operatorIdentifier:Expression * Expression * Expression
        | PrefixOperator of operatorIdentifier:Expression * Expression
        | AddressOf of singleAmpersand:bool * Expression
        | Wildcard
        | Variable of char
        | Identifier of string list
        | Constant of Constant
        | Parentheses of Expression
        | Lambda of LambdaArg list * LambdaBody
        | LambdaBody of Expression
        | LambdaArg of Expression
        | Tuple of Expression list
        | List of Expression list
        | Array of Expression list
        | If of cond:Expression * body:Expression * ``else``:Expression option
        | Else of Expression
        | Null
    and LambdaArg = LambdaArg of Expression
    and LambdaBody = LambdaBody of Expression

    type HintNode =
        | HintPat of Pattern
        | HintExpr of Expression

    type Suggestion =
        | Expr of Expression
        | Message of string

    type Hint =
        { MatchedNode:HintNode
          Suggestion:Suggestion }

    /// Provides a way of creating a single list from any number of hint ASTs.
    /// Means we can simply iterate over a single list for each node in the F# tree
    /// when matching hints rather than check each hint AST for each node.
    module MergeSyntaxTrees =

        open System.Collections.Generic

        type SyntaxHintNode =
            | Identifier = 1uy
            | Null = 2uy
            | Expression = 3uy
            | FuncApp = 4uy
            | Unit = 5uy
            | AddressOf = 6uy

            | If = 10uy
            | Else = 11uy

            | Lambda = 20uy
            | LambdaArg = 21uy
            | LambdaBody = 22uy

            | ArrayOrList = 30uy
            | Tuple = 31uy

            | Variable = 40uy
            | Wildcard = 41uy

            | ConstantBool = 51uy
            | ConstantByte = 52uy
            | ConstantChar = 53uy
            | ConstantDecimal = 54uy
            | ConstantDouble = 55uy
            | ConstantInt16 = 56uy
            | ConstantInt32 = 57uy
            | ConstantInt64 = 58uy
            | ConstantIntPtr = 59uy
            | ConstantSByte = 60uy
            | ConstantSingle = 61uy
            | ConstantString = 62uy
            | ConstantUInt16 = 63uy
            | ConstantUInt32 = 64uy
            | ConstantUInt64 = 65uy
            | ConstantUIntPtr = 66uy
            | ConstantBytes = 67uy
            | ConstantUserNum = 68uy

            | Cons = 101uy
            | And = 102uy
            | Or = 103uy

        [<NoComparison>]
        type Node =
            { Edges:Edges
              MatchedHint:Hint list }
        and [<CustomEquality; NoComparison>] Edges =
            { Lookup:Dictionary<int, Node>
              AnyMatch:(char option * Node) list }

            override lhs.Equals(other) =
                match other with
                | :? Edges as rhs ->
                    let getList dict = Seq.toList dict |> List.map (fun (x:KeyValuePair<_, _>) -> (x.Key, x.Value))

                    lhs.AnyMatch = rhs.AnyMatch &&
                    lhs.Lookup.Count = rhs.Lookup.Count &&
                    getList lhs.Lookup = getList rhs.Lookup
                | _ -> false

            override this.GetHashCode() = hash (this.AnyMatch, hash this.Lookup)

            static member Empty = { Lookup = Dictionary<_, _>(); AnyMatch = [] }

        let private getConstKey = function
            | Constant.Unit -> SyntaxHintNode.Unit
            | Constant.Bool(_) -> SyntaxHintNode.ConstantBool
            | Constant.Byte(_) -> SyntaxHintNode.ConstantByte
            | Constant.Bytes(_) -> SyntaxHintNode.ConstantBytes
            | Constant.Char(_) -> SyntaxHintNode.ConstantChar
            | Constant.Decimal(_) -> SyntaxHintNode.ConstantDecimal
            | Constant.Double(_) -> SyntaxHintNode.ConstantDouble
            | Constant.Int16(_) -> SyntaxHintNode.ConstantInt16
            | Constant.Int32(_) -> SyntaxHintNode.ConstantInt32
            | Constant.Int64(_) -> SyntaxHintNode.ConstantInt64
            | Constant.IntPtr(_) -> SyntaxHintNode.ConstantIntPtr
            | Constant.SByte(_) -> SyntaxHintNode.ConstantSByte
            | Constant.Single(_) -> SyntaxHintNode.ConstantSingle
            | Constant.String(_) -> SyntaxHintNode.ConstantString
            | Constant.UInt16(_) -> SyntaxHintNode.ConstantUInt16
            | Constant.UInt32(_) -> SyntaxHintNode.ConstantUInt32
            | Constant.UInt64(_) -> SyntaxHintNode.ConstantUInt64
            | Constant.UIntPtr(_) -> SyntaxHintNode.ConstantUIntPtr
            | Constant.UserNum(_) -> SyntaxHintNode.ConstantUserNum

        let rec private getExprKey = function
            | Expression.FunctionApplication(_)
            | Expression.InfixOperator(_)
            | Expression.PrefixOperator(_) -> SyntaxHintNode.FuncApp
            | Expression.AddressOf(_) -> SyntaxHintNode.AddressOf
            | Expression.Parentheses(expr) -> getExprKey expr
            | Expression.Lambda(_) -> SyntaxHintNode.Lambda
            | Expression.LambdaArg(_) -> SyntaxHintNode.LambdaArg
            | Expression.LambdaBody(_) -> SyntaxHintNode.LambdaBody
            | Expression.Tuple(_) -> SyntaxHintNode.Tuple
            | Expression.Constant(constant) -> getConstKey constant
            | Expression.List(_)
            | Expression.Array(_) -> SyntaxHintNode.ArrayOrList
            | Expression.If(_) -> SyntaxHintNode.If
            | Expression.Else(_) -> SyntaxHintNode.Else
            | Expression.Identifier(_) -> SyntaxHintNode.Identifier
            | Expression.Null -> SyntaxHintNode.Null
            | Expression.Wildcard -> SyntaxHintNode.Wildcard
            | Expression.Variable(_) -> SyntaxHintNode.Variable

        let rec private getPatternKey = function
            | Pattern.Cons(_) -> SyntaxHintNode.Cons
            | Pattern.Or(_) -> SyntaxHintNode.Or
            | Pattern.Wildcard -> SyntaxHintNode.Wildcard
            | Pattern.Variable(_) -> SyntaxHintNode.Variable
            | Pattern.Identifier(_) -> SyntaxHintNode.Identifier
            | Pattern.Constant(constant) -> getConstKey constant
            | Pattern.Parentheses(pattern) -> getPatternKey pattern
            | Pattern.Tuple(_) -> SyntaxHintNode.Tuple
            | Pattern.List(_)
            | Pattern.Array(_) -> SyntaxHintNode.ArrayOrList
            | Pattern.Null -> SyntaxHintNode.Null

        let rec private getKey = function
            | HintExpr(expr) -> getExprKey expr
            | HintPat(pattern) -> getPatternKey pattern

        let rec private getChildren = function
            | HintExpr(Expression.Parentheses(expr)) -> getChildren <| HintExpr expr
            | HintExpr(Expression.Lambda(args, LambdaBody(body))) ->
                [ for LambdaArg(arg) in args -> HintExpr arg
                  yield HintExpr body ]
            | HintExpr(Expression.LambdaArg(arg)) ->
                [HintExpr arg]
            | HintExpr(Expression.LambdaBody(body)) ->
                [HintExpr body]
            | HintExpr(Expression.InfixOperator(Expression.Identifier(["::"]) as ident, lhs, rhs)) ->
                [HintExpr ident; HintExpr (Expression.Tuple([lhs; rhs]))]
            | HintExpr(Expression.InfixOperator(ident, lhs, rhs)) ->
                [HintExpr ident; HintExpr lhs; HintExpr rhs]
            | HintExpr(Expression.PrefixOperator(ident, expr)) ->
                [HintExpr ident; HintExpr expr]
            | HintExpr(Expression.AddressOf(_, expr)) -> [HintExpr expr]
            | HintExpr(Expression.FunctionApplication(exprs))
            | HintExpr(Expression.Tuple(exprs))
            | HintExpr(Expression.List(exprs))
            | HintExpr(Expression.Array(exprs)) -> exprs |> List.map HintExpr
            | HintExpr(Expression.If(ifCond, bodyExpr, Some(elseExpr))) ->
                [HintExpr ifCond; HintExpr bodyExpr; HintExpr elseExpr]
            | HintExpr(Expression.If(ifCond, bodyExpr, None)) ->
                [HintExpr ifCond; HintExpr bodyExpr]
            | HintExpr(Expression.Else(x)) -> [HintExpr x]
            | HintExpr(Expression.Identifier(_))
            | HintExpr(Expression.Constant(_))
            | HintExpr(Expression.Null)
            | HintExpr(Expression.Wildcard)
            | HintExpr(Expression.Variable(_)) -> []
            | HintPat(Pattern.Cons(lhs, rhs))
            | HintPat(Pattern.Or(lhs, rhs)) -> [HintPat lhs; HintPat rhs]
            | HintPat(Pattern.Array(patterns))
            | HintPat(Pattern.List(patterns))
            | HintPat(Pattern.Tuple(patterns)) -> patterns |> List.map HintPat
            | HintPat(Pattern.Parentheses(pattern)) -> [HintPat pattern]
            | HintPat(Pattern.Variable(_))
            | HintPat(Pattern.Identifier(_))
            | HintPat(Pattern.Constant(_))
            | HintPat(Pattern.Wildcard)
            | HintPat(Pattern.Null) -> []

        let private getConstantHashCode = function
            | Constant.Bool(x) -> hash x
            | Constant.Byte(x) -> hash x
            | Constant.Bytes(x) -> hash x
            | Constant.Char(x) -> hash x
            | Constant.Decimal(x) -> hash x
            | Constant.Double(x) -> hash x
            | Constant.Int16(x) -> hash x
            | Constant.Int32(x) -> hash x
            | Constant.Int64(x) -> hash x
            | Constant.IntPtr(x) -> hash x
            | Constant.SByte(x) -> hash x
            | Constant.Single(x) -> hash x
            | Constant.String(x) -> hash x
            | Constant.UInt16(x) -> hash x
            | Constant.UInt32(x) -> hash x
            | Constant.UInt64(x) -> hash x
            | Constant.UIntPtr(x) -> hash x
            | Constant.UserNum(x, y) -> hash (x, y)
            | _ -> 0

        let private getIdentifierHashCode = function
            | identifier when (List.isEmpty >> not) identifier ->
                identifier
                |> Seq.last
                |> ExpressionUtilities.identAsCompiledOpName
                |> hash
            | _ -> 0

        let rec private getHashCode node =
            match node with
            | HintExpr(Expression.Identifier(identifier))
            | HintPat(Pattern.Identifier(identifier)) -> getIdentifierHashCode identifier
            | HintExpr(Expression.Constant(constant))
            | HintPat(Pattern.Constant(constant)) -> getConstantHashCode constant
            | HintExpr(Expression.Parentheses(expr)) -> getHashCode <| HintExpr expr
            | HintPat(Pattern.Parentheses(expr)) -> getHashCode <| HintPat expr
            | _ -> 0

        let private hintToList (hint:Hint) =
            let nodes = Queue<_>()

            let rec depthFirstTraversal expr depth =
                let children = getChildren expr

                nodes.Enqueue(expr, depth)

                for child in children do
                    depthFirstTraversal child (depth + 1)

            depthFirstTraversal hint.MatchedNode 0

            (nodes |> Seq.toList, hint)

        type private HintList = (HintNode * int) list * Hint

        type private TransposedNode =
            | HintNode of key:HintNode * depth:int * rest:HintList
            | EndOfHint of Hint

        /// Gets the head of each given list
        let private transposeHead hintLists =
            let rec transposeHead builtList = function
                | (((key, depth)::tail), hint)::rest ->
                    let restOfHintList = (tail, hint)
                    let next = HintNode(key, depth, restOfHintList)::builtList
                    transposeHead next rest
                | ([], hint)::rest ->
                    let next = EndOfHint(hint)::builtList
                    transposeHead next rest
                | [] -> builtList

            transposeHead [] hintLists

        let isAnyMatch = function
            | ((SyntaxHintNode.Wildcard | SyntaxHintNode.Variable), _, _, _) -> true
            | _ -> false

        let getHints items = items |> Seq.map (fun (_, _, _, hint) -> hint) |> Seq.toList

        let mergeHints hints =
            let rec getEdges transposed =
                let map = Dictionary<_, _>()

                transposed
                |> List.choose (function
                    | HintNode(expr, depth, rest) -> Some(getKey expr, expr, depth, rest)
                    | EndOfHint(_) -> None)
                |> List.filter (isAnyMatch >> not)
                |> Seq.groupBy (fun (key, expr, _, _) -> Utilities.hash2 key (getHashCode expr))
                |> Seq.iter (fun (hashcode, items) -> map.Add(hashcode, mergeHints (getHints items)))

                let anyMatches =
                    transposed
                    |> List.choose (function
                        | HintNode(expr, depth, rest) ->
                            match (getKey expr, expr) with
                            | (SyntaxHintNode.Wildcard as key), HintExpr(Expression.Wildcard)
                            | (SyntaxHintNode.Wildcard as key), HintPat(Pattern.Wildcard)
                            | (SyntaxHintNode.Variable as key), HintExpr(Expression.Variable(_))
                            | (SyntaxHintNode.Variable as key), HintPat(Pattern.Variable(_)) ->
                                Some(key, expr, depth, rest)
                            | _ -> None
                        | EndOfHint(_) -> None)
                    |> Seq.groupBy (fun (_, expr, _, _) -> expr)
                    |> Seq.choose
                        (fun (expr, items) ->
                            match expr with
                            | HintPat(Pattern.Wildcard)
                            | HintExpr(Expression.Wildcard) -> Some(None, mergeHints (getHints items))
                            | HintPat(Pattern.Variable(var))
                            | HintExpr(Expression.Variable(var)) -> Some(Some(var), mergeHints (getHints items))
                            | _ -> None)
                    |> Seq.toList

                { Lookup = map
                  AnyMatch = anyMatches }

            and mergeHints hints =
                let transposed = transposeHead hints

                let edges = getEdges transposed

                let matchedHints =
                    transposed
                    |> Seq.choose (function
                        | HintNode(_) -> None
                        | EndOfHint(hint) -> Some(hint))
                    |> Seq.toList

                { Edges = edges
                  MatchedHint = matchedHints }

            let transposed =
                hints |> List.map hintToList |> transposeHead

            getEdges transposed

    let charListToString charList =
        Seq.fold (fun x y -> x + y.ToString()) "" charList

    let pischar chars : Parser<char, 'T> =
        satisfy (fun x -> List.exists ((=) x) chars)

    let pnotchar chars : Parser<char, 'T> =
        satisfy (fun x -> not <| List.exists ((=) x) chars)

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
                let identStr = System.String.Join("", ident)

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
                  pident |>> fun x -> [x] ]

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
                  attempt (pidentorop |>> fun x -> [x])
                  plongident ]
            |>> List.map charListToString

    module StringAndCharacterLiterals =
        let private hexToCharacter hex =
            char(System.Convert.ToInt32(hex, 16))

        let private decimalToCharacter dec =
            char(System.Convert.ToInt32(dec, 10))

        let private escapeMap =
            [ ('"', '\"')
              ('\\', '\\')
              ('\'', '\'')
              ('n', '\n')
              ('t', '\t')
              ('b', '\b')
              ('r', '\r')
              ('a', '\a')
              ('f', '\f')
              ('v', '\v') ] |> Map.ofList

        let private pescapechar: Parser<char, unit> =
            skipChar '\\'
            >>. pischar ['"';'\\';'\'';'n';'t';'b';'r';'a';'f';'v']
            |>> fun x -> Map.find x escapeMap

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
            >>= fun x ->
                if x.Length <> 4 then
                    fail "Unicode graph short must be 4 hex characters long"
                else
                    preturn (x |> charListToString |> hexToCharacter)

        let private punicodegraphlong: Parser<char, unit> =
            skipString "\\U"
            >>. many1 hex
            >>= fun x ->
                if x.Length <> 8 then
                    fail "Unicode graph long must be 8 hex characters long"
                else
                    preturn (x |> charListToString |> hexToCharacter)

        let private ptrigraph: Parser<char, unit> =
            skipChar '\\'
            >>. many1 digit
            >>= fun x ->
                if x.Length <> 3 then
                    fail "Trigraph must be 3 characters long"
                else
                    preturn (x |> charListToString |> decimalToCharacter)

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
            |>> fun x -> '0'::'x'::x

        let private poctalint: Parser<char list, unit> =
            skipChar '0'
            >>. (skipChar 'o' <|> skipChar 'O')
            >>. many1 octal
            |>> fun x -> '0'::'o'::x

        let private pbinaryint: Parser<char list, unit> =
            skipChar '0'
            >>. (skipChar 'b' <|> skipChar 'B')
            >>. many1 (pchar '0' <|> pchar '1')
            |>> fun x -> '0'::'b'::x

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

        let ptuple (pparser:Parser<'T, unit>) : Parser<'T list, unit> =
            skipChar '('
            >>. pparser
            .>> skipChar ','
            .>>. sepEndBy1 pparser (skipChar ',')
            .>> skipChar ')'
            |>> fun (func, rest) -> (func::rest)

        let plist (pparser:Parser<'T, unit>): Parser<'T list, unit> =
            skipChar '['
            >>. spaces
            >>. sepEndBy pparser (skipChar ';')
            .>> spaces
            .>> skipChar ']'

        let parray (pparser:Parser<'T, unit>): Parser<'T list, unit> =
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
            |>> fun ((condition, expr), elseExpr) -> Expression.If(condition, expr, elseExpr |> Option.map Expression.Else)

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
                    (arguments |> List.map (Expression.LambdaArg >> LambdaArg),
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
                    notFollowedBy (pstring "==>") |>> fun _ -> ""
                else if prefix = "|" then
                    notFollowedBy (pstring "]") |>> fun _ -> ""
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
                    preturn ""

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
                    notFollowedBy (pstring "]") |>> fun _ -> ""
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
            let! m = Expressions.pexpression

            do! phintcenter

            let! s = psuggestion

            return { MatchedNode = HintExpr m; Suggestion = s }
        }

    let ppatternbasedhint =
        parse {
            let! m = Patterns.ppattern

            do! phintcenter

            let! s = psuggestion

            return { MatchedNode = HintPat m; Suggestion = s }
        }

    let phint: Parser<Hint, unit> =
        choice
            [ spaces >>. (skipString "pattern:") >>. spaces >>. ppatternbasedhint
              pexpressionbasedhint ]