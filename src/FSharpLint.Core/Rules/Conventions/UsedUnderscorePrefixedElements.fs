module FSharpLint.Rules.UsedUnderscorePrefixedElements

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules


let runner (args:AstNodeRuleParams) =
    
    let mutable error = Array.empty
    let mutable idents = List.empty<string>
    match args.AstNode with
        | AstNode.Expression (SynExpr.LetOrUse (isRecursive, isUse, bindings, body, range)) ->
            match bindings.[0] with
            | SynBinding(synAccessOption, synBindingKind, mustInline, isMutable, synAttributeLists, preXmlDoc, synValData, headPat, synBindingReturnInfoOption, synExpr, range, debugPointAtBinding) ->
                match headPat with
                | SynPat.Named(synPat, ident, isSelfIdentifier, synAccessOption, range) ->
                    idents <- List.append idents [ident.idText]
                | _ ->
                    ()
                    
            match body with
            | SynExpr.Sequential(debugPointAtSequential, isTrueSeq, synExpr, expr2, range) ->
                match synExpr with
                    | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) ->

                        match argExpr with
                            | SynExpr.Ident ident ->
                                if (List.contains ident.idText idents) then
                                    error <- ({
                                        Range = range
                                        Message = String.Format(Resources.GetString ("RulesUsedUnderscorePrefixedElements"))
                                        SuggestedFix = None
                                        TypeChecks = []
                                    } |> Array.singleton) 
                                else
                                    ()
                            | _ ->
                                ()
                    | _ ->
                        ()
                | _ ->
                    ()          
        | _ -> ()
        
    error

let rule =
    { Name = "UsedUnderscorePrefixedElements"
      Identifier = Identifiers.UsedUnderscorePrefixedElements
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule