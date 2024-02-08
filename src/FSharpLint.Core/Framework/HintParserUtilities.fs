namespace FSharpLint.Framework

open HintParserTypes

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
        { Edges: Edges; MatchedHint: Hint list }

    and [<CustomEquality; NoComparison>] Edges =
        {
            Lookup: Dictionary<int, Node>
            AnyMatch: (char option * Node) list
        }

        override this.Equals(other) =
            match other with
            | :? Edges as rhs ->
                let getList dict =
                    Seq.toList dict
                    |> List.map (fun (dictItems: KeyValuePair<_, _>) -> (dictItems.Key, dictItems.Value))

                this.AnyMatch = rhs.AnyMatch
                && this.Lookup.Count = rhs.Lookup.Count
                && getList this.Lookup = getList rhs.Lookup
            | _ -> false

        override this.GetHashCode() = hash (this.AnyMatch, hash this.Lookup)

        static member Empty =
            {
                Lookup = Dictionary<_, _>()
                AnyMatch = List.Empty
            }

    let private getConstKey =
        function
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

    let rec private getExprKey =
        function
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

    let rec private getPatternKey =
        function
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

    let rec private getKey =
        function
        | HintExpr(expr) -> getExprKey expr
        | HintPat(pattern) -> getPatternKey pattern

    let rec private getChildren =
        function
        | HintExpr(Expression.Parentheses(expr)) -> getChildren <| HintExpr expr
        | HintExpr(Expression.Lambda(args, LambdaBody(body))) ->
            [ for LambdaArg(arg) in args -> HintExpr arg
              yield HintExpr body ]
        | HintExpr(Expression.LambdaArg(arg)) -> [ HintExpr arg ]
        | HintExpr(Expression.LambdaBody(body)) -> [ HintExpr body ]
        | HintExpr(Expression.InfixOperator(Expression.Identifier([ "::" ]) as ident, lhs, rhs)) ->
            [ HintExpr ident; HintExpr(Expression.Tuple([ lhs; rhs ])) ]
        | HintExpr(Expression.InfixOperator(ident, lhs, rhs)) -> [ HintExpr ident; HintExpr lhs; HintExpr rhs ]
        | HintExpr(Expression.PrefixOperator(ident, expr)) -> [ HintExpr ident; HintExpr expr ]
        | HintExpr(Expression.AddressOf(_, expr)) -> [ HintExpr expr ]
        | HintExpr(Expression.FunctionApplication(exprs))
        | HintExpr(Expression.Tuple(exprs))
        | HintExpr(Expression.List(exprs))
        | HintExpr(Expression.Array(exprs)) -> List.map HintExpr exprs
        | HintExpr(Expression.If(ifCond, bodyExpr, Some(elseExpr))) ->
            [ HintExpr ifCond; HintExpr bodyExpr; HintExpr elseExpr ]
        | HintExpr(Expression.If(ifCond, bodyExpr, None)) -> [ HintExpr ifCond; HintExpr bodyExpr ]
        | HintExpr(Expression.Else(expression)) -> [ HintExpr expression ]
        | HintExpr(Expression.Identifier(_))
        | HintExpr(Expression.Constant(_))
        | HintExpr(Expression.Null)
        | HintExpr(Expression.Wildcard)
        | HintExpr(Expression.Variable(_)) -> List.Empty
        | HintPat(Pattern.Cons(lhs, rhs))
        | HintPat(Pattern.Or(lhs, rhs)) -> [ HintPat lhs; HintPat rhs ]
        | HintPat(Pattern.Array(patterns))
        | HintPat(Pattern.List(patterns))
        | HintPat(Pattern.Tuple(patterns)) -> List.map HintPat patterns
        | HintPat(Pattern.Parentheses(pattern)) -> [ HintPat pattern ]
        | HintPat(Pattern.Variable(_))
        | HintPat(Pattern.Identifier(_))
        | HintPat(Pattern.Constant(_))
        | HintPat(Pattern.Wildcard)
        | HintPat(Pattern.Null) -> List.Empty

    let private getConstantHashCode =
        function
        | Constant.Bool value -> hash value
        | Constant.Byte value -> hash value
        | Constant.Bytes value -> hash value
        | Constant.Char value -> hash value
        | Constant.Decimal value -> hash value
        | Constant.Double value -> hash value
        | Constant.Int16 value -> hash value
        | Constant.Int32 value -> hash value
        | Constant.Int64 value -> hash value
        | Constant.IntPtr value -> hash value
        | Constant.SByte value -> hash value
        | Constant.Single value -> hash value
        | Constant.String value -> hash value
        | Constant.UInt16 value -> hash value
        | Constant.UInt32 value -> hash value
        | Constant.UInt64 value -> hash value
        | Constant.UIntPtr value -> hash value
        | Constant.UserNum(intValue, charValue) -> hash (intValue, charValue)
        | _ -> 0

    let private getIdentifierHashCode =
        function
        | identifier when (List.isEmpty >> not) identifier ->
            match (Seq.tryLast identifier) with
            | Some value -> value |> ExpressionUtilities.identAsCompiledOpName |> hash
            | None -> failwith "There's no last element in identifier."
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

    let private hintToList (hint: Hint) =
        let nodes = Queue<_>()

        let rec depthFirstTraversal expr depth =
            let children = getChildren expr

            nodes.Enqueue(expr, depth)

            for child in children do
                depthFirstTraversal child (depth + 1)

        depthFirstTraversal hint.MatchedNode 0

        (Seq.toList nodes, hint)

    type private HintList = (HintNode * int) list * Hint

    type private TransposedNode =
        | HintNode of key: HintNode * depth: int * rest: HintList
        | EndOfHint of Hint

    /// Gets the head of each given list
    let private transposeHead hintLists =
        let rec transposeHead builtList =
            function
            | (((key, depth) :: tail), hint) :: rest ->
                let restOfHintList = (tail, hint)
                let next = HintNode(key, depth, restOfHintList) :: builtList
                transposeHead next rest
            | ([], hint) :: rest ->
                let next = EndOfHint(hint) :: builtList
                transposeHead next rest
            | [] -> builtList

        transposeHead List.Empty hintLists

    let isAnyMatch =
        function
        | ((SyntaxHintNode.Wildcard | SyntaxHintNode.Variable), _, _, _) -> true
        | _ -> false

    let getHints items =
        items |> Seq.map (fun (_, _, _, hint) -> hint) |> Seq.toList

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
                        | (SyntaxHintNode.Variable as key), HintPat(Pattern.Variable(_)) -> Some(key, expr, depth, rest)
                        | _ -> None
                    | EndOfHint(_) -> None)
                |> Seq.groupBy (fun (_, expr, _, _) -> expr)
                |> Seq.choose (fun (expr, items) ->
                    match expr with
                    | HintPat(Pattern.Wildcard)
                    | HintExpr(Expression.Wildcard) -> Some(None, mergeHints (getHints items))
                    | HintPat(Pattern.Variable(var))
                    | HintExpr(Expression.Variable(var)) -> Some(Some(var), mergeHints (getHints items))
                    | _ -> None)
                |> Seq.toList

            { Lookup = map; AnyMatch = anyMatches }

        and mergeHints hints =
            let transposed = transposeHead hints

            let edges = getEdges transposed

            let matchedHints =
                transposed
                |> Seq.choose (function
                    | HintNode(_) -> None
                    | EndOfHint(hint) -> Some(hint))
                |> Seq.toList

            {
                Edges = edges
                MatchedHint = matchedHints
            }

        let transposed = hints |> List.map hintToList |> transposeHead

        getEdges transposed
