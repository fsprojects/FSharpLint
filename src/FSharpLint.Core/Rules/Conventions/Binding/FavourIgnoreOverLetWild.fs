module FSharpLint.Rules.FavourIgnoreOverLetWild

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Ast
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
    match args.astNode with
    | AstNode.Binding(SynBinding.Binding(_, _, _, isMutable, _, _, _, pattern, _, expr, range, _))
            when Helper.Binding.isLetBinding args.nodeIndex args.syntaxArray args.skipArray ->
        checkForBindingToAWildcard pattern range
    | _ -> Array.empty

/// Checks if any code uses 'let _ = ...' and suggests to use the ignore function.
let rule =
    { name = "FavourIgnoreOverLetWild"
      identifier = Identifiers.FavourIgnoreOverLetWild
      ruleConfig = { AstNodeRuleConfig.runner = runner; cleanup = ignore } }
    |> AstNodeRule               
