module FSharpLint.Rules.SuggestUseAutoProperty

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | MemberDefinition
        (
            SynMemberDefn.Member
                (
                    SynBinding
                        (
                            accessibility,
                            kind,
                            _,
                            false,
                            _attributes,
                            _xmlDoc,
                            valData,
                            SynPat.LongIdent (_, _, _, argPats, _, _),
                            returnInfo,
                            expr,
                            bindingRange,
                            _
                        ),
                    memberRange
                )
        ) ->
        match (expr, argPats) with
        | (_, SynArgPats.Pats pats) when pats.Length > 0 -> // non-property member
            Array.empty
        | (SynExpr.Const (constant, range), _) ->
            { Range = memberRange
              Message = Resources.GetString "RulesSuggestUseAutoProperty"
              SuggestedFix = None
              TypeChecks = List.Empty }
            |> Array.singleton
        | (SynExpr.Ident _ident, _) ->
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
