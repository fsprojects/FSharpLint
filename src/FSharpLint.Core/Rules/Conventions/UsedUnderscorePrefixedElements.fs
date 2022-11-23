module FSharpLint.Rules.UsedUnderscorePrefixedElements

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private checkUsedIdent (previousIdent: Ident) (body: SynExpr) =
    match body with
    | SynExpr.Sequential(_debugPointAtSequential, _isTrueSeq, synExpr, _expr2, _range) ->
        match synExpr with
        | SynExpr.App(_exprAtomicFlag, _isInfix, _funcExpr, argExpr, range) ->
            match argExpr with
            | SynExpr.Ident ident ->
                if previousIdent.idText = ident.idText then
                    {
                        Range = range
                        Message = String.Format(Resources.GetString ("RulesUsedUnderscorePrefixedElements"))
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

let runner (args: AstNodeRuleParams) =
    
    let error =
        match args.AstNode with
        | AstNode.Expression (SynExpr.LetOrUse (_isRecursive, _isUse, bindings, body, _range)) ->
            match List.tryHead bindings with
            | Some(SynBinding(_synAccessOption, _synBindingKind, _mustInline, _isMutable, _synAttributeLists, _preXmlDoc, _synValData, headPat, _synBindingReturnInfoOption, _synExpr, _range, _debugPointAtBinding)) ->
                match headPat with
                | SynPat.Named(_synPat, ident, _isSelfIdentifier, _synAccessOption, _range) ->
                    if ident.idText.StartsWith "_" then
                        checkUsedIdent ident body
                    else
                        Array.empty
                | _ ->
                    Array.empty
            | _ -> Array.empty
        | _ -> Array.empty
        
    error

let rule =
    { Name = "UsedUnderscorePrefixedElements"
      Identifier = Identifiers.UsedUnderscorePrefixedElements
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
