module FSharpLint.Rules.SuggestUseAutoProperty

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let rec private isImmutableValueExpression (args: AstNodeRuleParams) (expression: SynExpr) =
    match expression with
    | SynExpr.Const (_constant, _range) -> true
    | SynExpr.Ident ident ->
        let isMutableVariable =
            let exists memberDef =
                match memberDef with
                | SynMemberDefn.LetBindings (bindings, _, _, _) ->
                    List.exists (fun (SynBinding (_, _, _, isMutable, _, _, _, headPat, _, _, _, _, _)) ->
                        match headPat with
                        | SynPat.Named (SynIdent(bindingIdent, _), _, _, _) when isMutable ->
                            bindingIdent.idText = ident.idText
                        | _ -> false) bindings
                | _ -> false

            match args.GetParents args.NodeIndex with
            | TypeDefinition (SynTypeDefn (_, SynTypeDefnRepr.ObjectModel (_, members, _), _, _, _, _)) :: _ ->
                List.exists exists members
            | _ -> false

        not isMutableVariable
    | SynExpr.ArrayOrList (_, elements, _) ->
        List.forall (isImmutableValueExpression args) elements
    | SynExpr.ArrayOrListComputed (_, innerExpr, _) ->
        isImmutableValueExpression args innerExpr
        || isImmutableSequentialExpression args innerExpr
    | _ -> false

and isImmutableSequentialExpression args expression =
    match expression with
    | SynExpr.Sequential (_, _, expr1, expr2, _, _) ->
        isImmutableValueExpression args expr1
        && (isImmutableSequentialExpression args expr2
            || isImmutableValueExpression args expr2)
    | _ -> false

let private hasStructAttribute node =
    match node with
    | AstNode.TypeDefinition(SynTypeDefn(SynComponentInfo(attributes, _, _, _, _, _, _, _), _, _, _, _, _)) ->
        attributes
        |> extractAttributes
        |> List.exists
            (fun attribute ->
                match List.tryLast attribute.TypeName.LongIdent with
                | Some(ident) -> ident.idText = "Struct" || ident.idText = "StructAttribute"
                | None -> false)
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
                            SynValData (Some memberFlags, _, _),
                            SynPat.LongIdent (memberIdentifier, _, _, argPats, _, _),
                            _returnInfo,
                            expr,
                            _bindingRange,
                            _,
                            _
                        ),
                    memberRange
                )
        ) when memberFlags.IsInstance ->
        match expr, argPats with
        | _, SynArgPats.Pats pats when pats.Length > 0 -> // non-property member
            Array.empty
        | expression, _ when isImmutableValueExpression args expression ->
            match args.GetParents args.NodeIndex with
            | parentNode :: _ when hasStructAttribute parentNode ->
                Array.empty
            | _ ->
                let suggestedFix =
                    lazy
                        (match memberIdentifier.LongIdent with
                            | [ _; memberName ] ->
                                Some
                                    { FromText = args.FileContent
                                      FromRange = memberIdentifier.Range
                                      ToText = $"val {memberName.idText}" }
                            | _ -> None)
            
                Array.singleton
                    { Range = memberRange
                      Message = Resources.GetString "RulesSuggestUseAutoProperty"
                      SuggestedFix = Some suggestedFix
                      TypeChecks = List.Empty }
        | _ -> Array.empty
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "SuggestUseAutoProperty"
            Identifier = Identifiers.SuggestUseAutoProperty
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
