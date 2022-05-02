module FSharpLint.Rules.FavourBasicControlFlow

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private isBasicControlFlow (synMatchClauses: List<SynMatchClause>) =
    let isFirstClauseTarget firstClause =
        match firstClause with
        | SynMatchClause(SynPat.Named(_, _, _, _, _), _, _, _, _)
        | SynMatchClause(SynPat.Const( _, _), _, _, _, _)
        | SynMatchClause(SynPat.LongIdent(LongIdentWithDots _, _, _, SynArgPats.Pats [], _, _), _, _, _, _) -> true
        | _ -> false

    let isLastClauseTarget lastClause =
        match lastClause with
        | SynMatchClause(SynPat.Wild _, None, _, _, _) -> true
        | _ -> false

    synMatchClauses.Length = 2 && isFirstClauseTarget synMatchClauses.[0] && isLastClauseTarget synMatchClauses.[1]

let runner args =
    match args.AstNode with
    | AstNode.Expression(SynExpr.Match (_, _, synMatchClauses, range)) when isBasicControlFlow synMatchClauses ->
        { Range = range
          Message = Resources.GetString "FavourBasicControlFlow"
          SuggestedFix = None
          TypeChecks = List.empty }
        |> Array.singleton
    | _ -> Array.empty

let rule =
    { Name = "FavourBasicControlFlow"
      Identifier = Identifiers.FavourBasicControlFlow
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
