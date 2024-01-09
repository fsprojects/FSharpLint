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

    let error =
        match args.AstNode with
        | AstNode.Binding (SynBinding(_synAcc, _synBinding, _mustInline, _isMut, _synAttribs, _preXmlDoc, _synValData, _headPat, _synBindingRet, synExpr, _range, _debugPointAtBinding, _)) ->
            match synExpr with
            | SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, _argExpr, _range) ->
                match funcExpr with
                | SynExpr.App(_exprAtomicFlag, _isInfix, funcExpr, argExpr, _range) ->
                    match funcExpr with
                    | ExpressionUtilities.Identifier([ ident ], _) ->
                        if ident.idText = "op_PipeRight" then
                            match argExpr with
                            | SynExpr.App(_exprAtomicFlag, _isInfix, _funcExpr, _argExpr, _range) ->
                                Array.empty 
                            | _ ->
                                errors ident.idRange
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
