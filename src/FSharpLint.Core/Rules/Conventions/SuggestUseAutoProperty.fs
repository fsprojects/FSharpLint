module FSharpLint.Rules.SuggestUseAutoProperty

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let rec private isImmutableValueExpression (args: AstNodeRuleParams) (expression: SynExpr) =
    match expression with
    | SynExpr.Const (constant, range) -> true
    | SynExpr.Ident ident ->
        let isMutableVariable =
            match args.GetParents args.NodeIndex with
            | TypeDefinition (SynTypeDefn (_, SynTypeDefnRepr.ObjectModel (_, members, _), _, _, _)) :: _ ->
                members
                |> List.exists (fun (memberDef: SynMemberDefn) ->
                    match memberDef with
                    | SynMemberDefn.LetBindings (bindings, _, _, _) ->
                        bindings
                        |> List.exists (fun (SynBinding (_, _, _, isMutable, _, _, _, headPat, _, _, _, _)) ->
                            match headPat with
                            | SynPat.Named (_, bindingIdent, _, _, _) when isMutable ->
                                bindingIdent.idText = ident.idText
                            | _ -> false)
                    | _ -> false)
            | _ -> false

        not isMutableVariable
    | SynExpr.ArrayOrList (_, elements, _) ->
        elements
        |> List.forall (isImmutableValueExpression args)
    | SynExpr.ArrayOrListOfSeqExpr (_, SynExpr.CompExpr(true, _, innerExpr, _), _) ->
        isImmutableValueExpression args innerExpr
        || isImmutableSequentialExpression args innerExpr
    | _ -> false

and isImmutableSequentialExpression args expression =
    match expression with
    | SynExpr.Sequential (_, _, expr1, expr2, _) ->
        isImmutableValueExpression args expr1
        && (isImmutableSequentialExpression args expr2
            || isImmutableValueExpression args expr2)
    | _ -> false

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | MemberDefinition
        (
            SynMemberDefn.Member
                (
                    SynBinding
                        (
                            _accessibility,
                            _kind,
                            _,
                            false,
                            _attributes,
                            _xmlDoc,
                            _valData,
                            SynPat.LongIdent (_, _, _, argPats, _, _),
                            _returnInfo,
                            expr,
                            _bindingRange,
                            _
                        ),
                    memberRange
                )
        ) ->
        match expr, argPats with
        | _, SynArgPats.Pats pats when pats.Length > 0 -> // non-property member
            Array.empty
        | expression, _ when isImmutableValueExpression args expression ->
            { Range = memberRange
              Message = Resources.GetString "RulesSuggestUseAutoProperty"
              SuggestedFix = None
              TypeChecks = List.Empty }
            |> Array.singleton
        | _ -> Array.empty
    | _ -> Array.empty

let rule =
    { Name = "SuggestUseAutoProperty"
      Identifier = Identifiers.SuggestUseAutoProperty
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
