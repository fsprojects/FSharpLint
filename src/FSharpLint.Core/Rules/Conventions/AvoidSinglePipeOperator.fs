module FSharpLint.Rules.AvoidSinglePipeOperator

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    let errors range =
        {
            Range = range
            Message = String.Format(Resources.GetString ("RulesAvoidSinglePipeOperator"))
            SuggestedFix = None
            TypeChecks = List.Empty
        } |> Array.singleton
    
    let rec checkExpr (expr: SynExpr) (parentList: AstNode list): WarningDetails array =
        let checkParentPiped (expr: AstNode) =
            match expr with
            | AstNode.Expression(SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, _argExpr, _range)) ->
                (checkExpr funcExpr []).Length = 0
            | _ -> false

        match expr with
        | SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, argExpr, _range) ->
            match funcExpr with
            | ExpressionUtilities.Identifier([ ident ], _) ->
                if ident.idText = "op_PipeRight" then
                    match argExpr with
                    | SynExpr.App(_exprAtomicFlag, _isInfix, _funcExpr, _argExpr, _range) ->
                        Array.empty
                    | SynExpr.IfThenElse _ ->
                        Array.empty
                    | _ ->
                        let isParentPiped =
                            match parentList with
                            | head::_ -> checkParentPiped head
                            | [] -> false
                        if isParentPiped then
                            Array.empty
                        else
                            errors ident.idRange
                else
                    Array.empty
            | _ ->
                Array.empty
        | _ ->
            Array.empty

    let error =
        match args.AstNode with
        | AstNode.Expression(SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, _argExpr, _range)) ->
            checkExpr funcExpr (args.GetParents args.NodeIndex)
        | _ ->
            Array.empty

    error


let rule =
    { Name = "AvoidSinglePipeOperator"
      Identifier = Identifiers.AvoidSinglePipeOperator
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
