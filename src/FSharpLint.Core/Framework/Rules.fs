module FSharpLint.Framework.Rules

open FSharpLint.Framework.Ast
open FSharpLint.Framework.Analyser

type AstNodeRuleParams<'Config> =
    { astNode : AstNode 
      info : AnalyserInfo
      config : 'Config }

type Rule<'Params> =
  { name : string
    identifier : string option
    runner : 'Params -> LintSuggestion [] }
