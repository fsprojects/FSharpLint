module FSharpLint.Rules.NestedStatements

open System
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<RequireQualifiedAccess>]
type Config = { Depth:int }

let private violationText (depth:int) =
    let violationTextFormatString = Resources.GetString "RulesNestedStatementsViolation"
    String.Format(violationTextFormatString, depth)

/// Lambda wildcard arguments are named internally as _argN, a match is then generated for them in the AST.
/// e.g. fun _ -> () is represented in the AST as fun _arg1 -> match _arg1 with | _ -> ().
/// This function returns true if the given match statement is compiler generated for a lambda wildcard argument.
let private isCompilerGeneratedMatch = function
    | SynExpr.Match(_, SynExpr.Ident(ident), _, _, _) when ident.idText.StartsWith("_arg") -> true
    | _ -> false

let private areChildrenNested = function
    | AstNode.Binding(SynBinding(_))
    | AstNode.Expression(SynExpr.Lambda(_))
    | AstNode.Expression(SynExpr.MatchLambda(_))
    | AstNode.Expression(SynExpr.IfThenElse(_))
    | AstNode.Expression(SynExpr.Lazy(_))
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

let private getRange node =
    match node with 
    | AstNode.Expression(node) -> Some node.Range
    | AstNode.Binding(node) -> Some node.RangeOfBindingWithRhs
    | _ -> None

let private distanceToCommonParent (syntaxArray:AbstractSyntaxArray.Node []) iIndex jIndex =
    let mutable iIndex = iIndex
    let mutable jIndex = jIndex
    let mutable distance = 0

    while iIndex <> jIndex do
        if iIndex > jIndex then
            iIndex <- syntaxArray.[iIndex].ParentIndex

            if iIndex <> jIndex && areChildrenNested syntaxArray.[iIndex].Actual then
                distance <- distance + 1
        else
            jIndex <- syntaxArray.[jIndex].ParentIndex

    distance

/// Is node a duplicate of a node in the AST containing ExtraSyntaxInfo
/// e.g. lambda arg being a duplicate of the lambda.
let isMetaData args node index =
    let parentIndex = args.SyntaxArray.[index].ParentIndex
    if parentIndex = index then false
    else
        Object.ReferenceEquals(node, args.SyntaxArray.[parentIndex].Actual)

let isElseIf args node index =
    match node with
    | AstNode.Expression(SynExpr.IfThenElse(_)) ->
        let parentIndex = args.SyntaxArray.[index].ParentIndex
        if parentIndex = index then false
        else
            match args.SyntaxArray.[parentIndex].Actual with
            | AstNode.Expression(SynExpr.IfThenElse(_, _, Some(_), _, _, _, _)) -> true
            | _ -> false
    | _ -> false

let mutable depth = 0

let decrementDepthToCommonParent args iIndex jIndex =
    if jIndex < args.SyntaxArray.Length then
        // If next node in array is not a sibling or child of the current node.
        let parent = args.SyntaxArray.[jIndex].ParentIndex
        if parent <> iIndex && parent <> args.SyntaxArray.[iIndex].ParentIndex then
            // Decrement depth until we reach a common parent.
            depth <- depth - (distanceToCommonParent args.SyntaxArray iIndex jIndex)

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
        let index = args.NodeIndex
        let node = args.AstNode
        decrementDepthToCommonParent args index (index + 1)

        if areChildrenNested node && not <| isMetaData args node index && not <| isElseIf args node index then
            if depth >= config.Depth then
                // Skip children as we've had a violation containing them.
                let skipChildren = index + args.SyntaxArray.[index].NumberOfChildren + 1
                decrementDepthToCommonParent args index skipChildren
                skipToIndex <- Some skipChildren

                getRange node
                |> Option.map (fun range ->
                    {
                        Range = range
                        Message = violationText config.Depth
                        SuggestedFix = None
                        TypeChecks = List.Empty
                    })
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
    AstNodeRule
        {
            Name = "NestedStatements"
            Identifier = Identifiers.NestedStatements
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = cleanup
                }
        }
