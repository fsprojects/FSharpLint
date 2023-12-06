module FSharpLint.Rules.WildcardNamedWithAsPattern

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private checkForWildcardNamedWithAsPattern pattern =
    match pattern with
    | SynPat.Wild(range) ->
        { Range = range
          Message = Resources.GetString("RulesWildcardNamedWithAsPattern")
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton
    | _ -> Array.empty

let private runner (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Pattern(SynPat.Wild(_) as pattern) ->
        checkForWildcardNamedWithAsPattern pattern
    | _ -> Array.empty

/// Checks if any code uses 'let _ = ...' and suggests to use the ignore function.
let rule =
    { Name = "WildcardNamedWithAsPattern"
      Identifier = Identifiers.WildcardNamedWithAsPattern
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule

