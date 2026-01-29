module FSharpLint.Rules.RecommendIfElseConstructOverMatch

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner args =
    let isBasicControlFlow (synMatchClauses: List<SynMatchClause>) =
        let (|Wildcard|_|) clause =
            match clause with
            | SynMatchClause(SynPat.Wild _, None, _, _, _, _) -> true
            | _ -> false

        match synMatchClauses with
        | [ SynMatchClause(SynPat.Null(_), _, _, _, _, _); Wildcard ]
        | [ SynMatchClause(SynPat.Const(_), _, _, _, _, _); Wildcard ]
        | [ SynMatchClause(SynPat.Named(SynIdent.SynIdent(_), _, _, _), None, _, _, _, _); Wildcard ]
        | [ SynMatchClause(SynPat.LongIdent(SynLongIdent(_), _, _, SynArgPats.Pats [], _, _), _, _, _, _, _); Wildcard ] -> 
            true
        | _ -> false
    
    match args.AstNode with
    | AstNode.Expression(SynExpr.Match (_, _, synMatchClauses, range, _)) when isBasicControlFlow synMatchClauses ->
        { Range = range
          Message = Resources.GetString "RecommendIfElseConstructOverMatch"
          SuggestedFix = None
          TypeChecks = List.empty }
        |> Array.singleton
    | _ -> Array.empty

let rule =
    { Name = "RecommendIfElseConstructOverMatch"
      Identifier = Identifiers.RecommendIfElseConstructOverMatch
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
