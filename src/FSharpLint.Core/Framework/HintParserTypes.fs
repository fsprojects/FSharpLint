namespace FSharpLint.Framework

module HintParserTypes =

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
