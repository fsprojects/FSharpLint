module FSharpLint.Rules.IndexerAccessorStyleConsistency

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework
open System

type IndexerAccessorStyle =
    | OCaml
    | CSharp

[<RequireQualifiedAccess>]
type Config = {
    Style: IndexerAccessorStyle
}

let generateOutput (range: FSharp.Compiler.Text.Range) (style: IndexerAccessorStyle) =
    Array.singleton
        {
            Range = range
            Message = String.Format(Resources.GetString "RulesIndexerAccessorStyleConsistency", style.ToString())
            SuggestedFix = None
            TypeChecks = List.Empty
        }

let runner (config: Config) (args: AstNodeRuleParams) =
    match config.Style with
    | IndexerAccessorStyle.OCaml ->
        match args.AstNode with
        | AstNode.Expression (SynExpr.App (ExprAtomicFlag.Atomic, _, SynExpr.Ident _, SynExpr.ArrayOrListComputed (_, _expr, range), _)) ->
            generateOutput range IndexerAccessorStyle.OCaml
        | _ -> 
            Array.empty
    | IndexerAccessorStyle.CSharp ->
        match args.AstNode with
        | AstNode.Expression (SynExpr.DotIndexedGet (_, _, _, range)) ->
            generateOutput range IndexerAccessorStyle.CSharp
        | _ -> 
            Array.empty

let rule config =
    AstNodeRule
        {
            Name = "IndexerAccessorStyleConsistency"
            Identifier = Identifiers.IndexerAccessorStyleConsistency
            RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore }
        }
