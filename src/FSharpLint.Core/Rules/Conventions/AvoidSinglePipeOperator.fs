module FSharpLint.Rules.AvoidSinglePipeOperator

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    let error =
        match args.AstNode with
        | AstNode.Binding (SynBinding(_synAcc, _synBinding, _mustInline, _isMut, _synAttribs, _preXmlDoc, _synValData, _headPat, _synBindingRet, synExpr, _range, _debugPointAtBinding)) ->
            match synExpr with
            | SynExpr.App(_exprAtomicFlag, _isInfix, outerFuncExpr, outerArgExpr, appRange) ->
                match outerFuncExpr with
                | SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, argExpr, pipeRange) ->
                    match funcExpr with
                    | SynExpr.Ident ident ->
                        if ident.idText = "op_PipeRight" then
                            match argExpr with
                            | SynExpr.App(_exprAtomicFlag, _isInfix, _funcExpr, _argExpr, _range) ->
                                Array.empty 
                            | _ ->
                                let suggestedFix = lazy(
                                    let maybeFuncText = ExpressionUtilities.tryFindTextOfRange outerArgExpr.Range args.FileContent
                                    let maybeArgText = ExpressionUtilities.tryFindTextOfRange argExpr.Range args.FileContent
                                    match maybeFuncText, maybeArgText with
                                    | Some(funcText), Some(argText) ->
                                        let replacementText = sprintf "%s %s" funcText argText
                                        Some { FromText=args.FileContent; FromRange=appRange; ToText=replacementText }
                                    | _ -> None)
                                Array.singleton
                                    {
                                        Range = pipeRange
                                        Message = String.Format(Resources.GetString ("RulesAvoidSinglePipeOperator"))
                                        SuggestedFix = Some suggestedFix
                                        TypeChecks = List.Empty
                                    }
                        else
                            Array.empty
                    | _ ->
                        Array.empty
                | _ ->
                    Array.empty
            | _ ->
                Array.empty
        | _ ->
            Array.empty

    error


let rule =
    { Name = "AvoidSinglePipeOperator"
      Identifier = Identifiers.AvoidSinglePipeOperator
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
