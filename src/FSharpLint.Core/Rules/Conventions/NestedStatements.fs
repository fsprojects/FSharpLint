module FSharpLint.Rules.NestedStatements

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<RequireQualifiedAccess>]
type Config =
    {
        // fsharplint:disable RecordFieldNames
        depth : int
        // fsharplint:enable RecordFieldNames
    }

let private error (depth:int) =
    let errorFormatString = Resources.GetString("RulesNestedStatementsError")
    String.Format(errorFormatString, depth)

/// Lambda wildcard arguments are named internally as _argN, a match is then generated for them in the AST.
/// e.g. fun _ -> () is represented in the AST as fun _arg1 -> match _arg1 with | _ -> ().
/// This function returns true if the given match statement is compiler generated for a lmabda wildcard argument.
let private isCompilerGeneratedMatch = function
    | SynExpr.Match(_, SynExpr.Ident(ident), _, _) when ident.idText.StartsWith("_arg") -> true
    | _ -> false

let private areChildrenNested = function
    | AstNode.Binding(SynBinding.Binding(_))
    | AstNode.Expression(SynExpr.Lambda(_))
    | AstNode.Expression(SynExpr.MatchLambda(_))
    | AstNode.Expression(SynExpr.IfThenElse(_))
    | AstNode.Expression(SynExpr.Lazy(_))
    | AstNode.Expression(SynExpr.Record(_))
    | AstNode.Expression(SynExpr.ObjExpr(_))
    | AstNode.Expression(SynExpr.TryFinally(_))
    | AstNode.Expression(SynExpr.TryWith(_))
    | AstNode.Expression(SynExpr.Tuple(_))
    | AstNode.Expression(SynExpr.Quote(_))
    | AstNode.Expression(SynExpr.While(_))
    | AstNode.Expression(SynExpr.For(_))
    | AstNode.Expression(SynExpr.ForEach(_)) -> true
    | AstNode.Expression(SynExpr.Match(_) as matchExpr) when not (isCompilerGeneratedMatch matchExpr) -> true
    | _ -> false

let private getRange = function
    | AstNode.Expression(node) -> Some node.Range
    | AstNode.Binding(node) -> Some node.RangeOfBindingAndRhs
    | _ -> None

let private distanceToCommonParent (syntaxArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) i j =
    let mutable i = i
    let mutable j = j
    let mutable distance = 0

    while i <> j do
        if i > j then
            i <- skipArray.[i].ParentIndex

            if i <> j && areChildrenNested syntaxArray.[i].Actual then
                distance <- distance + 1
        else
            j <- skipArray.[j].ParentIndex

    distance

/// Is node a duplicate of a node in the AST containing ExtraSyntaxInfo
/// e.g. lambda arg being a duplicate of the lamdba.
let isMetaData args node i =
    let parentIndex = args.SkipArray.[i].ParentIndex
    if parentIndex = i then false
    else
        Object.ReferenceEquals(node, args.SyntaxArray.[parentIndex].Actual)

let isElseIf args node i =
    match node with
    | AstNode.Expression(SynExpr.IfThenElse(_)) ->
        let parentIndex = args.SkipArray.[i].ParentIndex
        if parentIndex = i then false
        else
            match args.SyntaxArray.[parentIndex].Actual with
            | AstNode.Expression(SynExpr.IfThenElse(_, _, Some(_), _, _, _, _)) -> true
            | _ -> false
    | _ -> false

let mutable depth = 0

let decrementDepthToCommonParent args i j =
    if j < args.SyntaxArray.Length then
        // If next node in array is not a sibling or child of the current node.
        let parent = args.SkipArray.[j].ParentIndex
        if parent <> i && parent <> args.SkipArray.[i].ParentIndex then
            // Decrement depth until we reach a common parent.
            depth <- depth - (distanceToCommonParent args.SyntaxArray args.SkipArray i j)

let mutable skipToIndex = None

let runner (config:Config) (args:AstNodeRuleParams) =
    let skip =
        match skipToIndex with
        | Some skipTo when skipTo = args.NodeIndex ->
            skipToIndex <- None
            false
        | None ->
            false
        | _ ->
            true

    if not skip then
        let i = args.NodeIndex
        let node = args.AstNode
        decrementDepthToCommonParent args i (i + 1)

        if areChildrenNested node && not <| isMetaData args node i && not <| isElseIf args node i then
            if depth >= config.depth then
                // Skip children as we've had an error containing them.
                let skipChildren = i + args.SkipArray.[i].NumberOfChildren + 1
                decrementDepthToCommonParent args i skipChildren
                skipToIndex <- Some skipChildren

                getRange node
                |> Option.map (fun range ->
                    { Range = range; Message = error config.depth; SuggestedFix = None; TypeChecks = [] })
                |> Option.toArray
            else
                depth <- depth + 1
                Array.empty
        else
            Array.empty
    else
        Array.empty

let cleanup () =
    depth <- 0
    skipToIndex <- None

let rule config =
    { Name = "NestedStatements"
      Identifier = Identifiers.NestedStatements
      RuleConfig = { AstNodeRuleConfig.Runner = runner config
                     Cleanup = cleanup } }
    |> AstNodeRule
