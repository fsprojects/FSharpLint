module FSharpLint.Rules.AsyncExceptionWithoutReturn

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities

[<TailCall>]
let rec checkExpression (expression: SynExpr) (range: range) (continuation: unit -> array<WarningDetails>) =
    Array.append
        (match expression with
        | SynExpr.Sequential (_, _, firstExpression, secondExpression, _, _) ->
            checkExpression
                firstExpression
                range
                (fun () -> (checkExpression secondExpression secondExpression.Range returnEmptyArray))
        | SynExpr.Paren (innerExpression, _, _, innerRange) -> checkExpression innerExpression innerRange returnEmptyArray
        | SynExpr.While (_, _, innerExpression, innerRange) -> checkExpression innerExpression innerRange returnEmptyArray
        | SynExpr.For (_, _, _, _, _, _, _, innerExpression, innerRange) -> checkExpression innerExpression innerRange returnEmptyArray
        | SynExpr.ForEach (_, _, _, _, _, _, innerExpression, innerRange) -> checkExpression innerExpression innerRange returnEmptyArray
        | SynExpr.Match (_, _, clauses, _range, _) ->
            let subExpressions = clauses |> List.map (fun (SynMatchClause (_, _, clause, innerRange, _, _)) -> (clause, innerRange))
            checkMultipleExpressions subExpressions returnEmptyArray
        | SynExpr.Do (innerExpression, innerRange) -> checkExpression innerExpression innerRange returnEmptyArray
        | SynExpr.TryWith (tryExpression, withCases, tryRange, _, _, _) ->
            let subExpressions =
                withCases |> List.map (fun (SynMatchClause (_, _, withCase, withRange, _, _)) -> (withCase, withRange))
            checkMultipleExpressions subExpressions (fun () -> checkExpression tryExpression tryRange returnEmptyArray)
        | SynExpr.TryFinally (tryExpression, finallyExpr, innerRange, _, _, _) ->
            checkExpression tryExpression range (fun () -> checkExpression finallyExpr innerRange returnEmptyArray)
        | SynExpr.IfThenElse (_, thenExpr, elseExpr, _, _, innerRange, _) ->
            checkExpression
                thenExpr
                innerRange
                (fun () -> 
                    match elseExpr with
                    | Some elseExpression ->
                        checkExpression elseExpression innerRange returnEmptyArray
                    | None -> Array.empty)
            
        | SynExpr.App (_, _, SynExpr.Ident failwithId, _, _) when
            failwithId.idText = "failwith"
            || failwithId.idText = "failwithf"
            || failwithId.idText = "raise"
            ->
            Array.singleton
                {
                    Range = range
                    Message = Resources.GetString "RulesAsyncExceptionWithoutReturn"
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }
        | SynExpr.App (_, _, funcExpr, _, innerRange) ->
            checkExpression funcExpr innerRange returnEmptyArray
        | SynExpr.LetOrUse (_, _, _, body, innerRange, _) ->
            checkExpression body innerRange returnEmptyArray
        | _ -> Array.empty)
        (continuation ())
and [<TailCall>] checkMultipleExpressions (expressions: list<SynExpr * range>) (continuation: unit -> array<WarningDetails>) =
    match expressions with
    | (expression, innerRange) :: tail ->
        checkExpression
            expression
            innerRange
            (fun () -> 
                checkMultipleExpressions
                    tail
                    continuation)
    | [] -> 
        continuation ()

let runner args =
    match args.AstNode with
    | AstNode.Expression
        (
            SynExpr.App (_, _, (SynExpr.Ident compExprName), (SynExpr.ComputationExpr (_, innerExpression, _)), range)
        ) when compExprName.idText = "async" -> checkExpression innerExpression range returnEmptyArray
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "AsyncExceptionWithoutReturn"
            Identifier = Identifiers.AsyncExceptionWithoutReturn
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
