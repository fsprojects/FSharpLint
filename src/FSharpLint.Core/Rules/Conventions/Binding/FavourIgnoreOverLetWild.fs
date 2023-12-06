module FSharpLint.Rules.FavourIgnoreOverLetWild

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private checkForBindingToAWildcard pattern range =
    let rec findWildAndIgnoreParens = function
        | SynPat.Paren(pattern, _) -> findWildAndIgnoreParens pattern
        | SynPat.Wild(_) -> true
        | _ -> false

    if findWildAndIgnoreParens pattern then
        { Range = range
          Message = Resources.GetString("RulesFavourIgnoreOverLetWildError")
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton
    else
        Array.empty

let private runner (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, pattern, _, _, range, _, _))
            when Helper.Binding.isLetBinding args.NodeIndex args.SyntaxArray ->
        checkForBindingToAWildcard pattern range
    | _ -> Array.empty

/// Checks if any code uses 'let _ = ...' and suggests to use the ignore function.
let rule =
    { Name = "FavourIgnoreOverLetWild"
      Identifier = Identifiers.FavourIgnoreOverLetWild
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
