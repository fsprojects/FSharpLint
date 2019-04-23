module FSharpLint.Rules.WildcardNamedWithAsPattern

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private checkForWildcardNamedWithAsPattern pattern =
    match pattern with
    | SynPat.Named(SynPat.Wild(wildcardRange), _, _, _, range) when wildcardRange <> range ->
        { Range = range
          Message = Resources.GetString("RulesWildcardNamedWithAsPattern")
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton
    | _ -> Array.empty

let private runner (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.Pattern(SynPat.Named(SynPat.Wild(_), _, _, _, _) as pattern) ->
        checkForWildcardNamedWithAsPattern pattern
    | _ -> Array.empty

/// Checks if any code uses 'let _ = ...' and suggests to use the ignore function.
let rule =
    { name = "WildcardNamedWithAsPattern"
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner; cleanup = ignore } }
    |> AstNodeRule                
    
