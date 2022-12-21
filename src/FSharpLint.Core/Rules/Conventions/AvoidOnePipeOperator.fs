module FSharpLint.Rules.AvoidOnePipeOperator

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    printfn "%A" args.AstNode
    printfn "-------"
    
    let error =
        match args.AstNode with
        | AstNode.Binding (SynBinding(synAccessOption, synBindingKind, mustInline, isMutable, synAttributeLists, preXmlDoc, synValData, headPat, synBindingReturnInfoOption, synExpr, range, debugPointAtBinding)) ->
            match synExpr with
            | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) ->
                match funcExpr with
                | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) ->
                    match funcExpr with
                    | SynExpr.Ident ident ->
                        if ident.idText = "op_PipeRight" then
                            match argExpr with
                            | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) ->
                                
                                match funcExpr with
                                | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) ->
                                    match funcExpr with
                                    | SynExpr.Ident ident ->
                                        if ident.idText = "op_PipeRight" then
                                            Array.empty
                                        else
                                            {
                                                Range = range
                                                Message = String.Format(Resources.GetString ("RulesAvoidOnePipeOperator"))
                                                SuggestedFix = None
                                                TypeChecks = List.Empty
                                            } |> Array.singleton
                                    | _ ->
                                        {
                                            Range = range
                                            Message = String.Format(Resources.GetString ("RulesAvoidOnePipeOperator"))
                                            SuggestedFix = None
                                            TypeChecks = List.Empty
                                        } |> Array.singleton
                                        
                                | SynExpr.Ident _ ->
                                    Array.empty
                                
                                | _ ->
                                    {
                                        Range = range
                                        Message = String.Format(Resources.GetString ("RulesAvoidOnePipeOperator"))
                                        SuggestedFix = None
                                        TypeChecks = List.Empty
                                    } |> Array.singleton
                            | _ ->
                                {
                                    Range = range
                                    Message = String.Format(Resources.GetString ("RulesAvoidOnePipeOperator"))
                                    SuggestedFix = None
                                    TypeChecks = List.Empty
                                } |> Array.singleton
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
    { Name = "AvoidOnePipeOperator"
      Identifier = Identifiers.AvoidOnePipeOperator
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule