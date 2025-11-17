module FSharpLint.Rules.InterpolatedStringWithNoSubstitution

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let checkInterpolatedString (contents: list<SynInterpolatedStringPart>) range =
    failwith "Not yet implemented"

let runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.InterpolatedString(contents, _synStringKind, range)) ->
        checkInterpolatedString contents range
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "InterpolatedStringWithNoSubstitution"
            Identifier = Identifiers.InterpolatedStringWithNoSubstitution
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
