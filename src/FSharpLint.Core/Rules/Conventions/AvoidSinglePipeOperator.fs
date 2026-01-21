module FSharpLint.Rules.AvoidSinglePipeOperator

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let isSingleLine (range: FSharp.Compiler.Text.range) =
    range.EndLine = range.StartLine
   
type private CheckExprContinuationPassingStyleArgs =
    {
        Args: AstNodeRuleParams
        Expr: SynExpr
        OuterArgExpr: SynExpr
        Range: FSharp.Compiler.Text.range
        ParentList: list<AstNode>
        Continuation: array<WarningDetails> -> array<WarningDetails>
    }

[<TailCall>]   
let rec private checkParentPipedContinuationPassingStyle (args: AstNodeRuleParams) (expr: AstNode) outerArgExpr range cont =
    match expr with
    | AstNode.Expression(SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, _argExpr, _range)) ->
        checkExprContinuationPassingStyle 
            {
                Args = args
                Expr = funcExpr
                OuterArgExpr = outerArgExpr
                Range = range
                ParentList = List.Empty
                Continuation = fun result -> cont (Seq.isEmpty result)
            }
    | _ -> cont false

and [<TailCall>] private checkExprContinuationPassingStyle (arguments: CheckExprContinuationPassingStyleArgs) =
    let errors range suggestedFix =
        Array.singleton
            {
                Range = range
                Message = String.Format(Resources.GetString ("RulesAvoidSinglePipeOperator"))
                SuggestedFix = suggestedFix
                TypeChecks = List.Empty
            }

    match arguments.Expr with
    | SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, argExpr, _appRange) when isSingleLine argExpr.Range ->
        match funcExpr with
        | ExpressionUtilities.Identifier([ ident ], _) ->
            if ident.idText = "op_PipeRight" then
                match argExpr with
                | SynExpr.App(_exprAtomicFlag, _isInfix, _funcExpr, _argExpr, _range) ->
                    arguments.Continuation Array.empty
                | SynExpr.IfThenElse _ ->
                    arguments.Continuation Array.empty
                | _ ->
                    match arguments.ParentList with
                    | head::_ -> 
                        let continuation isParentPiped =
                            if isParentPiped then
                                arguments.Continuation Array.empty
                            else
                                let suggestedFix = lazy(
                                    let maybeFuncText = ExpressionUtilities.tryFindTextOfRange arguments.OuterArgExpr.Range arguments.Args.FileContent
                                    let maybeArgText = ExpressionUtilities.tryFindTextOfRange argExpr.Range arguments.Args.FileContent
                                    match (maybeFuncText, maybeArgText) with
                                    | Some(funcText), Some(argText) ->
                                        let replacementText = sprintf "%s %s" funcText argText
                                        Some { FromText=arguments.Args.FileContent; FromRange=arguments.Range; ToText=replacementText }
                                    | _ -> None)
                                arguments.Continuation (errors ident.idRange (Some suggestedFix))
                        checkParentPipedContinuationPassingStyle arguments.Args head arguments.OuterArgExpr arguments.Range continuation
                    | [] -> 
                        let suggestedFix = lazy(
                            let maybeFuncText = ExpressionUtilities.tryFindTextOfRange arguments.OuterArgExpr.Range arguments.Args.FileContent
                            let maybeArgText = ExpressionUtilities.tryFindTextOfRange argExpr.Range arguments.Args.FileContent
                            match (maybeFuncText, maybeArgText) with
                            | Some(funcText), Some(argText) ->
                                let replacementText = sprintf "%s %s" funcText argText
                                Some { FromText=arguments.Args.FileContent; FromRange=arguments.Range; ToText=replacementText }
                            | _ -> None)
                        arguments.Continuation (errors ident.idRange (Some suggestedFix))
            else
                arguments.Continuation Array.empty
        | _ ->
            arguments.Continuation Array.empty
    | _ ->
        arguments.Continuation Array.empty

let runner (args: AstNodeRuleParams) =
    let checkExpr (expr: SynExpr) (outerArgExpr: SynExpr) (range: FSharp.Compiler.Text.range) (parentList: AstNode list): WarningDetails array =
        checkExprContinuationPassingStyle 
            { 
                Args = args
                Expr = expr
                OuterArgExpr = outerArgExpr
                Range = range
                ParentList = parentList
                Continuation = id
            }

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
    AstNodeRule
        {
            Name = "AvoidSinglePipeOperator"
            Identifier = Identifiers.AvoidSinglePipeOperator
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
