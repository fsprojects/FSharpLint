module FSharpLint.Rules.AsyncExceptionWithoutReturn

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities

[<TailCall>]
let rec checkExpression (expression: SynExpr) (range: range) (continuation: unit -> array<ViolationDetails>) =
    Array.append
        (match expression with
        | SynExpr.Sequential (_, _, firstExpression, secondExpression, _, _) ->
            checkExpression
                firstExpression
                range
                (fun () -> (checkExpression secondExpression secondExpression.Range returnEmptyArray))
        | SynExpr.Paren (innerExpression, _, _, range) -> checkExpression innerExpression range returnEmptyArray
        | SynExpr.While (_, _, innerExpression, range) -> checkExpression innerExpression range returnEmptyArray
        | SynExpr.For (_, _, _, _, _, _, _, innerExpression, range) -> checkExpression innerExpression range returnEmptyArray
        | SynExpr.ForEach (_, _, _, _, _, _, innerExpression, range) -> checkExpression innerExpression range returnEmptyArray
        | SynExpr.Match (_, _, clauses, range, _) ->
            let subExpressions = clauses |> List.map (fun (SynMatchClause (_, _, clause, range, _, _)) -> (clause, range))
            checkMultipleExpressions subExpressions returnEmptyArray
        | SynExpr.Do (innerExpression, range) -> checkExpression innerExpression range returnEmptyArray
        | SynExpr.TryWith (tryExpression, withCases, tryRange, _, _, _) ->
            let subExpressions =
                withCases |> List.map (fun (SynMatchClause (_, _, withCase, withRange, _, _)) -> (withCase, withRange))
            checkMultipleExpressions subExpressions (fun () -> checkExpression tryExpression tryRange returnEmptyArray)
        | SynExpr.TryFinally (tryExpression, finallyExpr, range, _, _, _) ->
            checkExpression tryExpression range (fun () -> checkExpression finallyExpr range returnEmptyArray)
        | SynExpr.IfThenElse (_, thenExpr, elseExpr, _, _, range, _) ->
            checkExpression
                thenExpr
                range
                (fun () -> 
                    match elseExpr with
                    | Some elseExpression ->
                        checkExpression elseExpression range returnEmptyArray
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
                    AutoFix = None
                    TypeChecks = List.Empty
                }
        | SynExpr.App (_, _, funcExpr, _, range) ->
            checkExpression funcExpr range returnEmptyArray
        | SynExpr.LetOrUse (_, _, _, body, range, _) ->
            checkExpression body range returnEmptyArray
        | _ -> Array.empty)
        (continuation ())
and [<TailCall>] checkMultipleExpressions (expressions: list<SynExpr * range>) (continuation: unit -> array<ViolationDetails>) =
    match expressions with
    | (expression, range) :: tail -> 
        checkExpression
            expression
            range
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
