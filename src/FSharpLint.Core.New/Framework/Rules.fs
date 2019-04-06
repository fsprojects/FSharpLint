module FSharpLint.Framework.Rules

open FSharpLint.Framework.Ast
open FSharpLint.Framework.Analyser

type AstNodeRuleParams =
    { astNode : AstNode
      getParents : int -> AstNode list 
      fileContent : string }

type RuleWithParams<'Params> =
  { name : string
    identifier : string option
    runner : 'Params -> LintSuggestion [] }
