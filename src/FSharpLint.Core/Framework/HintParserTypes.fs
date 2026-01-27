namespace FSharpLint.Framework

module HintParserTypes =

    type Constant =
        | Byte of value: byte
        | Bytes of value: byte[]
        | Char of value: char
        | Decimal of value: decimal
        | Double of value: double
        | Int16 of value: int16
        | Int32 of value: int32
        | Int64 of value: int64
        | IntPtr of value: nativeint
        | SByte of value: sbyte
        | Single of value: single
        | UInt16 of value: uint16
        | UInt32 of value: uint32
        | UInt64 of value: uint64
        | UIntPtr of value: unativeint
        | UserNum of value: bigint * suffix: char
        | String of value: string
        | Unit
        | Bool of value: bool

    [<RequireQualifiedAccess>]
    type Pattern =
        | Cons of lhs: Pattern * rhs: Pattern
        | Or of lhs: Pattern * rhs: Pattern
        | Wildcard
        | Variable of name: char
        | Identifier of nameParts: string list
        | Constant of value: Constant
        | Parentheses of innerHint: Pattern
        | Tuple of hints: Pattern list
        | List of hints: Pattern list
        | Array of hints: Pattern list
        | Null

    [<RequireQualifiedAccess>]
    type Expression =
        | FunctionApplication of hints: Expression list
        | InfixOperator of operatorIdentifier:Expression * leftHint: Expression * rightHint: Expression
        | PrefixOperator of operatorIdentifier:Expression * hint: Expression
        | AddressOf of singleAmpersand:bool * hint: Expression
        | Wildcard
        | Variable of varChar: char
        | Identifier of hints: string list
        | Constant of constant: Constant
        | Parentheses of hint: Expression
        | Lambda of args: LambdaArg list * body: LambdaBody
        | LambdaBody of body: Expression
        | LambdaArg of arg: Expression
        | Tuple of hints: Expression list
        | List of hints:Expression list
        | Array of hints:Expression list
        | If of cond:Expression * body:Expression * ``else``:Expression option
        | Else of hint: Expression
        | Null
    and LambdaArg = LambdaArg of Expression
    and LambdaBody = LambdaBody of Expression

    type HintNode =
        | HintPat of hint: Pattern
        | HintExpr of hint: Expression

    type Suggestion =
        | Expr of expression: Expression
        | Message of message: string

    type Hint =
        { MatchedNode:HintNode
          Suggestion:Suggestion }
