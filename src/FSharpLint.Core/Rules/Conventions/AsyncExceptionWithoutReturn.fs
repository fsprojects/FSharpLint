module FSharpLint.Rules.AsyncExceptionWithoutReturn

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let isIdentifierExecptionRaise (identifer: Ident) =
    identifer.idText = "failwith"
    || identifer.idText = "failwithf"
    || identifer.idText = "raise"
    

let checkSequentialExpression (expression: SynExpr) (range: range) =
    match expression with
    | SynExpr.App (_, _, SynExpr.Ident failwithId, _, _) when
        isIdentifierExecptionRaise failwithId
        ->
        { Range = range
          Message = Resources.GetString "RulesAsyncExceptionWithoutReturn"
          SuggestedFix = None
          TypeChecks = List.Empty }
        |> Array.singleton
    // WIP
    //| SynExpr.YieldOrReturn (_, SynExpr.App (_, _, SynExpr.Ident identifier, _, _), range) when 
    //    isIdentifierExecptionRaise identifier -> 

    | _ -> Array.empty


let rec checkExpression (expression: SynExpr) (range: range) =
    match expression with
    | SynExpr.Sequential (_, _, firstExpression, secondExpression, _) ->
        let result = checkSequentialExpression firstExpression range
        Array.append result (checkExpression secondExpression secondExpression.Range)
    | SynExpr.Paren (innerExpression, _, _, range) -> checkExpression innerExpression range
    | SynExpr.While (_, _, innerExpression, range) -> checkExpression innerExpression range
    | SynExpr.For (_, _, _, _, _, innerExpression, range) -> checkExpression innerExpression range
    | SynExpr.ForEach (_, _, _, _, _, innerExpression, range) -> checkExpression innerExpression range
    | SynExpr.Match (_, _, clauses, range) ->
        clauses
        |> List.map (fun (SynMatchClause (_, _, clause, range, _)) -> checkExpression clause range)
        |> List.toArray
        |> Array.concat
    | SynExpr.Do (innerExpression, range) -> checkExpression innerExpression range
    | SynExpr.TryWith (tryExpression, tryRange, withCases, _, _, _, _) ->
        withCases
        |> List.map (fun (SynMatchClause (_, _, withCase, withRange, _)) -> checkExpression withCase withRange)
        |> List.toArray
        |> Array.concat
        |> Array.append (checkExpression tryExpression tryRange)
    | SynExpr.TryFinally (tryExpression, finallyExpr, range, _, _) ->
        checkExpression finallyExpr range
        |> Array.append (checkExpression tryExpression range)
    | SynExpr.IfThenElse (_, thenExpr, elseExpr, _, _, _, range) ->
        let checkThen = checkExpression thenExpr range

        match elseExpr with
        | Some elseExpression ->
            checkThen
            |> Array.append (checkExpression elseExpression range)
        | None -> checkThen
    | _ -> Array.empty


let runner args =
    printf "________%A" args.AstNode
    match args.AstNode with
    | AstNode.Expression
        (
            SynExpr.App (_, _, (SynExpr.Ident compExprName), (SynExpr.CompExpr (_, _, innerExpression, _)), range)
        ) when compExprName.idText = "async" -> checkExpression innerExpression range
    | _ -> Array.empty

let rule =
    { Name = "AsyncExceptionWithoutReturn"
      Identifier = Identifiers.AsyncExceptionWithoutReturn
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
