module FSharpLint.Rules.WildcardNamedWithAsPattern

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let rule =
    let runner (args:AstNodeRuleParams) =
        let checkForWildcardNamedWithAsPattern fileContents pattern =
            match pattern with
            | SynPat.As(SynPat.Wild(wildcardRange), SynPat.Named(SynIdent(identifier, _), _, _, _), range)
                when wildcardRange <> range ->
                let suggestedFix = 
                    lazy(
                        Some { FromRange = range; FromText = fileContents; ToText = identifier.idText })
                Array.singleton
                    { Range = range
                      Message = Resources.GetString("RulesWildcardNamedWithAsPattern")
                      SuggestedFix = Some suggestedFix
                      TypeChecks = List.Empty }
            | _ -> Array.empty

        match args.AstNode with
        | AstNode.Pattern(SynPat.As(SynPat.Wild(_), SynPat.Named(_), _) as pattern) ->
            checkForWildcardNamedWithAsPattern args.FileContent pattern
        | _ -> Array.empty

    AstNodeRule
        {
            Name = "WildcardNamedWithAsPattern"
            Identifier = Identifiers.WildcardNamedWithAsPattern
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
