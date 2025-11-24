module FSharpLint.Rules.IndexerAccessorStyleConsistency

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework
open System

[<RequireQualifiedAccess>]
type Config = {
    Style: string
}

let generateOutput (range: FSharp.Compiler.Text.Range) msg =
    Array.singleton
        {
            Range = range
            Message = Resources.GetString msg
            SuggestedFix = None
            TypeChecks = List.Empty
        }

let runner (config: Config) (args: AstNodeRuleParams) =
    let styleType = config.Style
    if String.Equals (styleType, "ocaml", StringComparison.InvariantCultureIgnoreCase) then
        match args.AstNode with
        | AstNode.Binding binding ->
            match binding with
            | SynBinding (_, _, _, _, _, _, _, SynPat.Named _, _,
                 SynExpr.App (ExprAtomicFlag.Atomic, _, SynExpr.Ident _, SynExpr.ArrayOrListComputed (_, expr, range), _), 
                 _, _, _)
                ->
                generateOutput range "RulesIndexerAccessorStyleConsistencyToOCaml"
            | _ ->
                Array.empty
        | _ -> 
            Array.empty
    elif String.Equals (styleType, "csharp", StringComparison.InvariantCultureIgnoreCase) then
        match args.AstNode with
        | AstNode.Binding binding ->
            match binding with
            | SynBinding (_, _, _, _, _, _, _, SynPat.Named _, _
                , SynExpr.DotIndexedGet (_, _, _, range), _, _, _) ->
                generateOutput range "RulesIndexerAccessorStyleConsistencyToCSharp"
            | _ ->
                Array.empty
        | _ -> 
            Array.empty
    else
        failwithf "Unknown style type %s" styleType

let rule config =
    AstNodeRule
        {
            Name = "IndexerAccessorStyleConsistency"
            Identifier = Identifiers.IndexerAccessorStyleConsistency
            RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore }
        }
