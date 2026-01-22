module FSharpLint.Rules.DiscourageStringInterpolationWithStringFormat

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner args =
    let isStringFormat (identifiers: List<Ident>) =
        "String" = identifiers.[0].idText && "Format" = identifiers.[1].idText

    let emitViolation range =
        { Range = range
          Message = Resources.GetString "RulesDiscourageStringInterpolationWithStringFormat"
          SuggestedFix = None
          TypeChecks = List.empty }
        |> Array.singleton

    match args.AstNode with
    | AstNode.Expression(SynExpr.App(_, _, SynExpr.LongIdent(_, SynLongIdent(ids, _, _), _, _), paren, range)) when ids.Length = 2 && isStringFormat ids ->
        match paren with
        | SynExpr.Paren(SynExpr.Tuple(_, SynExpr.Const(SynConst.String(_), _) :: _args, _, _), _, _, _) ->
            emitViolation range
        | SynExpr.Paren(SynExpr.Tuple(_, SynExpr.Ident(identifier) :: _args, _, _), _, _, _range)  ->
            let isBindingOfIdentifierToTemplate binding =
                match binding with
                | SynBinding(_, _, _, _, _, _, _, SynPat.Named(SynIdent.SynIdent(bindingIdent, _), _, _, _), _, SynExpr.Const(SynConst.String(value, _, _), _), _, _, _) -> 
                    value.Contains "{0}" && bindingIdent.idText = identifier.idText
                | _ -> false

            let hasDeclarationContainingBindingOfIdentifierToTemplate astNode =
                match astNode with
                | AstNode.ModuleOrNamespace(SynModuleOrNamespace(_, _, _, declarations, _, _, _, _, _)) ->
                    declarations
                    |> List.exists 
                        (fun decl ->
                            match decl with
                            | SynModuleDecl.Let(_, bindings, _) ->
                                bindings |> List.exists isBindingOfIdentifierToTemplate
                            | _ -> false)
                | _ -> false
            
            let parents = args.GetParents args.NodeIndex
            if parents |> List.exists hasDeclarationContainingBindingOfIdentifierToTemplate then
                emitViolation range
            else
                Array.empty
        | _ -> Array.empty
    | _ -> Array.empty

let rule =
    { Name = "DiscourageStringInterpolationWithStringFormat"
      Identifier = Identifiers.DiscourageStringInterpolationWithStringFormat
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
