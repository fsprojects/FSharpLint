module FSharpLint.Rules.SuggestUseAutoProperty

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities

[<TailCall>]
let rec private isImmutableValueExpression (args: AstNodeRuleParams) (expression: SynExpr) (continuation: bool -> bool) =
    match expression with
    | SynExpr.Const (_constant, _range) -> continuation true
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

        not isMutableVariable |> continuation
    | SynExpr.ArrayOrList (_, elements, _) ->
        areImmutableAllValueExpressions args elements id
    | SynExpr.ArrayOrListComputed (_, innerExpr, _) ->
        isImmutableValueExpression
            args
            innerExpr
            (fun prevResult -> prevResult || isImmutableSequentialExpression args innerExpr id)
    | _ -> continuation false

and [<TailCall>] areImmutableAllValueExpressions (args: AstNodeRuleParams) (expressions: list<SynExpr>) (continuation: bool -> bool) =
    match expressions with
    | head::tail ->
        areImmutableAllValueExpressions
            args
            tail
            (fun prevResult ->
                isImmutableValueExpression args head (fun prevResult2 -> prevResult2 && prevResult))
    | [] -> continuation true

and [<TailCall>] isImmutableSequentialExpression args expression (continuation: bool -> bool) =
    match expression with
    | SynExpr.Sequential (_, _, expr1, expr2, _, _) ->
        isImmutableValueExpression
            args
            expr1
            (fun prevResult -> 
                isImmutableSequentialExpression args expr2 
                        (fun prevResult2 ->
                            isImmutableValueExpression args expr2
                                (fun prevResult3 -> prevResult && prevResult2 || prevResult3)
                        )
            )
    | _ -> continuation false

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
        match (expr, argPats) with
        | _, SynArgPats.Pats pats when pats.Length > 0 -> // non-property member
            Array.empty
        | expression, _ when isImmutableValueExpression args expression id ->
            match args.GetParents args.NodeIndex with
            | parentNode :: _ when hasStructAttribute parentNode ->
                Array.empty
            | _ ->
                let autoFix =
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
                      AutoFix = Some autoFix
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
