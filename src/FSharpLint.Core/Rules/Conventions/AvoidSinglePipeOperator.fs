module FSharpLint.Rules.AvoidSinglePipeOperator

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    let errors range suggestedFix =
        {
            Range = range
            Message = String.Format(Resources.GetString ("RulesAvoidSinglePipeOperator"))
            SuggestedFix = suggestedFix
            TypeChecks = List.Empty
        } |> Array.singleton
    
    let rec checkExpr (expr: SynExpr) (outerArgExpr: SynExpr) (range: FSharp.Compiler.Text.range) (parentList: AstNode list): WarningDetails array =
        let checkParentPiped (expr: AstNode) =
            match expr with
            | AstNode.Expression(SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, _argExpr, _range)) ->
                checkExpr funcExpr outerArgExpr range List.Empty |> Seq.isEmpty
            | _ -> false

        match expr with
        | SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, argExpr, appRange) ->
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
                            let suggestedFix = lazy(
                                let maybeFuncText = ExpressionUtilities.tryFindTextOfRange outerArgExpr.Range args.FileContent
                                let maybeArgText = ExpressionUtilities.tryFindTextOfRange argExpr.Range args.FileContent
                                match maybeFuncText, maybeArgText with
                                | Some(funcText), Some(argText) ->
                                    let replacementText = sprintf "%s %s" funcText argText
                                    Some { FromText=args.FileContent; FromRange=range; ToText=replacementText }
                                | _ -> None)
                            errors ident.idRange (Some suggestedFix)
                else
                    Array.empty
            | _ ->
                Array.empty
        | _ ->
            Array.empty

    let error =
        match args.AstNode with
        | AstNode.Expression(SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, argExpr, range)) ->
            match argExpr with
            | SynExpr.App(_) ->
                // function has extra arguments
                Array.empty
            | _ ->
                checkExpr funcExpr argExpr range (args.GetParents args.NodeIndex)
        | _ ->
            Array.empty

    error


let rule =
    { Name = "AvoidSinglePipeOperator"
      Identifier = Identifiers.AvoidSinglePipeOperator
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
