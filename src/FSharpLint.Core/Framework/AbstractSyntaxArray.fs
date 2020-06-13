namespace FSharpLint.Framework

module AbstractSyntaxArray =

    open System.Collections.Generic
    open System.Diagnostics
    open FSharp.Compiler.SyntaxTree

    open Ast

    type SyntaxNode =
        | Identifier = 1uy
        | Null = 2uy
        | Expression = 3uy
        | FuncApp = 4uy
        | Unit = 5uy
        | AddressOf = 6uy
        | Paren = 7uy

        | If = 10uy
        | Else = 11uy

        | Lambda = 20uy
        | LambdaArg = 21uy
        | LambdaBody = 22uy

        | ArrayOrList = 30uy
        | Tuple = 31uy

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

        | ModuleOrNamespace = 70uy
        | ModuleDeclaration = 71uy
        | Binding = 72uy
        | TypeDefinition = 74uy
        | Field = 75uy
        | Type = 76uy
        | Match = 77uy
        | EnumCase = 78uy
        | UnionCase = 79uy
        | MemberDefinition = 80uy
        | ExceptionRepresentation = 81uy
        | TypeSimpleRepresentation = 82uy

        | Cons = 101uy
        | And = 102uy
        | Or = 103uy
        | Pattern = 104uy

        | Other = 255uy

    let private constToSyntaxNode = function
        | SynConst.Unit(_) -> SyntaxNode.Unit
        | SynConst.Bool(_) -> SyntaxNode.ConstantBool
        | SynConst.Byte(_) -> SyntaxNode.ConstantByte
        | SynConst.Bytes(_) -> SyntaxNode.ConstantBytes
        | SynConst.Char(_) -> SyntaxNode.ConstantChar
        | SynConst.Decimal(_) -> SyntaxNode.ConstantDecimal
        | SynConst.Double(_) -> SyntaxNode.ConstantDouble
        | SynConst.Int16(_) -> SyntaxNode.ConstantInt16
        | SynConst.Int32(_) -> SyntaxNode.ConstantInt32
        | SynConst.Int64(_) -> SyntaxNode.ConstantInt64
        | SynConst.IntPtr(_) -> SyntaxNode.ConstantIntPtr
        | SynConst.SByte(_) -> SyntaxNode.ConstantSByte
        | SynConst.Single(_) -> SyntaxNode.ConstantSingle
        | SynConst.String(_) -> SyntaxNode.ConstantString
        | SynConst.UInt16(_) -> SyntaxNode.ConstantUInt16
        | SynConst.UInt32(_) -> SyntaxNode.ConstantUInt32
        | SynConst.UInt64(_) -> SyntaxNode.ConstantUInt64
        | SynConst.UIntPtr(_) -> SyntaxNode.ConstantUIntPtr
        | SynConst.UInt16s(_)
        | SynConst.UserNum(_)
        | SynConst.Measure(_) -> SyntaxNode.Other

    let private astNodeToSyntaxNode = function
        | Expression(SynExpr.Null(_)) -> SyntaxNode.Null
        | Expression(SynExpr.Tuple(_)) -> SyntaxNode.Tuple
        | Expression(SynExpr.ArrayOrListOfSeqExpr(_))
        | Expression(SynExpr.ArrayOrList(_)) -> SyntaxNode.ArrayOrList
        | Expression(SynExpr.AddressOf(_)) -> SyntaxNode.AddressOf
        | Identifier(_) -> SyntaxNode.Identifier
        | Expression(SynExpr.App(_)) -> SyntaxNode.FuncApp
        | Expression(SynExpr.Lambda(_)) -> SyntaxNode.Lambda
        | Expression(SynExpr.IfThenElse(_)) -> SyntaxNode.If
        | Expression(SynExpr.Const(constant, _)) -> constToSyntaxNode constant
        | Expression(SynExpr.Ident(_) | SynExpr.LongIdent(_) | SynExpr.LongIdentSet(_)) -> SyntaxNode.Other
        | Expression(SynExpr.Paren(_)) -> SyntaxNode.Paren
        | Expression(_) -> SyntaxNode.Expression
        | Pattern(SynPat.Ands(_)) -> SyntaxNode.And
        | Pattern(SynPat.Or(_)) -> SyntaxNode.Or
        | Pattern(Cons(_)) -> SyntaxNode.Cons
        | Pattern(SynPat.Wild(_)) -> SyntaxNode.Wildcard
        | Pattern(SynPat.Const(constant, _)) -> constToSyntaxNode constant
        | Pattern(SynPat.ArrayOrList(_)) -> SyntaxNode.ArrayOrList
        | Pattern(SynPat.Tuple(_)) -> SyntaxNode.Tuple
        | Pattern(_) -> SyntaxNode.Pattern
        | ModuleOrNamespace(_) -> SyntaxNode.ModuleOrNamespace
        | ModuleDeclaration(_) -> SyntaxNode.ModuleDeclaration
        | AstNode.Binding(_) -> SyntaxNode.Binding
        | TypeDefinition(_) -> SyntaxNode.TypeDefinition
        | AstNode.Field(_) -> SyntaxNode.Field
        | Type(_) -> SyntaxNode.Type
        | Match(_) -> SyntaxNode.Match
        | MemberDefinition(_) -> SyntaxNode.MemberDefinition
        | ExceptionRepresentation(_) -> SyntaxNode.ExceptionRepresentation
        | TypeSimpleRepresentation(_) -> SyntaxNode.TypeSimpleRepresentation
        | TypeParameter(_)
        | ConstructorArguments(_)
        | SimplePattern(_)
        | SimplePatterns(_)
        | InterfaceImplementation(_)
        | TypeRepresentation(_)
        | File(_)
        | LambdaArg(_)
        | LambdaBody(_) 
        | Else(_) 
        | ComponentInfo(_) -> SyntaxNode.Other
        | EnumCase(_) -> SyntaxNode.EnumCase
        | UnionCase(_) -> SyntaxNode.UnionCase

    [<Struct; NoEquality; NoComparison; DebuggerDisplay("{DebuggerDisplay,nq}")>]
    type Node(hashcode: int, actual: AstNode) =
        member __.Hashcode = hashcode
        member __.Actual = actual

        member private __.DebuggerDisplay = "AstNode: " + string actual

    [<Struct>]
    type private PossibleSkip(skipPosition: int, depth: int) =
        member __.SkipPosition = skipPosition
        member __.Depth = depth

    /// We just want a hash of the last identifier.
    let rec private getIdentHash = function
        | [ident] -> hash ident
        | _::rest -> getIdentHash rest
        | [] -> 0

    /// Get hash code of an ast node to be used for the fuzzy match of hints against the ast.
    let private getHashCode node =
        match node with
        | Identifier(idents) -> getIdentHash idents
        | Pattern(SynPat.Const(SynConst.Bool(x), _))
        | Expression(SynExpr.Const(SynConst.Bool(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.Byte(x), _))
        | Expression(SynExpr.Const(SynConst.Byte(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.Bytes(x, _), _))
        | Expression(SynExpr.Const(SynConst.Bytes(x, _), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.Char(x), _))
        | Expression(SynExpr.Const(SynConst.Char(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.Decimal(x), _))
        | Expression(SynExpr.Const(SynConst.Decimal(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.Double(x), _))
        | Expression(SynExpr.Const(SynConst.Double(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.Int16(x), _))
        | Expression(SynExpr.Const(SynConst.Int16(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.Int32(x), _))
        | Expression(SynExpr.Const(SynConst.Int32(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.Int64(x), _))
        | Expression(SynExpr.Const(SynConst.Int64(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.IntPtr(x), _))
        | Expression(SynExpr.Const(SynConst.IntPtr(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.SByte(x), _))
        | Expression(SynExpr.Const(SynConst.SByte(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.Single(x), _))
        | Expression(SynExpr.Const(SynConst.Single(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.String(x, _), _))
        | Expression(SynExpr.Const(SynConst.String(x, _), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.UInt16(x), _))
        | Expression(SynExpr.Const(SynConst.UInt16(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.UInt16s(x), _))
        | Expression(SynExpr.Const(SynConst.UInt16s(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.UInt32(x), _))
        | Expression(SynExpr.Const(SynConst.UInt32(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.UInt64(x), _))
        | Expression(SynExpr.Const(SynConst.UInt64(x), _)) -> hash x
        | Pattern(SynPat.Const(SynConst.UIntPtr(x), _))
        | Expression(SynExpr.Const(SynConst.UIntPtr(x), _)) -> hash x
        | _ -> 0

    [<Struct; NoEquality; NoComparison>]
    type private StackedNode(node: AstNode, depth: int) =
        member __.Node = node
        member __.Depth = depth

    [<Struct; DebuggerDisplay("{DebuggerDisplay,nq}")>]
    type Skip(numberOfChildren: int, parentIndex: int) =
        member __.NumberOfChildren = numberOfChildren
        member __.ParentIndex = parentIndex

        member private __.DebuggerDisplay =
            "Skip: NumberOfChildren=" + string numberOfChildren + ", ParentIndex=" + string parentIndex

    /// Keep index of position so skip array can be created in the correct order.
    [<Struct>]
    type private TempSkip(numberOfChildren: int, parentIndex: int, index: int) =
        member __.NumberOfChildren = numberOfChildren
        member __.Index = index
        member __.ParentIndex = parentIndex

    /// Contains information on the current node being visited.
    [<NoEquality; NoComparison>]
    type CurrentNode =
        { Node:AstNode
          ChildNodes:AstNode list

          /// A list of parent nodes e.g. parent, grand parent, grand grand parent.
          Breadcrumbs:AstNode list }

    let astToArray ast =
        let astRoot = File ast

        let nodes = List<_>()
        let left = Stack<_>()
        let possibleSkips = Stack<PossibleSkip>()
        let skips = List<_>()

        let tryAddPossibleSkips depth =
            while possibleSkips.Count > 0 && possibleSkips.Peek().Depth >= depth do
                let nodePosition = possibleSkips.Pop().SkipPosition
                let numberOfChildren = nodes.Count - nodePosition - 1
                let parentIndex = if possibleSkips.Count > 0 then possibleSkips.Peek().SkipPosition else 0
                skips.Add(TempSkip(numberOfChildren, parentIndex, nodePosition))

        left.Push (StackedNode(astRoot, 0))

        while left.Count > 0 do
            let stackedNode = left.Pop()
            let node = stackedNode.Node
            let depth = stackedNode.Depth

            tryAddPossibleSkips depth
            
            // Strip out "extra info".
            let node =
                let extractExtraInfo actual extraInfoNode =
                    possibleSkips.Push (PossibleSkip(nodes.Count, depth))
                    nodes.Add (Node(Utilities.hash2 extraInfoNode 0, actual))
                    actual

                match node with
                | LambdaArg(arg) -> extractExtraInfo (SimplePatterns(arg)) SyntaxNode.LambdaArg
                | LambdaBody(body) -> extractExtraInfo (Expression(body)) SyntaxNode.LambdaBody
                | Else(body) -> extractExtraInfo (Expression(body)) SyntaxNode.Else
                | _ -> node

            traverseNode node (fun node -> left.Push (StackedNode(node, depth + 1)))

            match astNodeToSyntaxNode node with
            | SyntaxNode.Other -> ()
            | syntaxNode ->
                possibleSkips.Push (PossibleSkip(nodes.Count, depth))
                nodes.Add (Node(Utilities.hash2 syntaxNode (getHashCode node), node))

        tryAddPossibleSkips 0

        let skipArray = Array.zeroCreate skips.Count

        let mutable i = 0
        while i < skips.Count do
            let skip = skips.[i]
            skipArray.[skip.Index] <- Skip(skip.NumberOfChildren, skip.ParentIndex)

            i <- i + 1

        (nodes.ToArray(), skipArray)

    let getBreadcrumbs maxBreadcrumbs (syntaxArray:Node []) (skipArray:Skip []) i =
        let rec getBreadcrumbs breadcrumbs i =
            if i = 0 then
                let node = syntaxArray.[i].Actual
                node::breadcrumbs
            else if i < skipArray.Length && (List.length breadcrumbs) < maxBreadcrumbs then
                let node = syntaxArray.[i].Actual
                let parenti = skipArray.[i].ParentIndex
                getBreadcrumbs (node::breadcrumbs) parenti
            else
                breadcrumbs

        if i = 0 then []
        else getBreadcrumbs [] (skipArray.[i].ParentIndex) |> List.rev
