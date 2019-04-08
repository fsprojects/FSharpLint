module FSharpLint.Framework.Rules

open FSharpLint.Framework.Ast
open FSharpLint.Framework.Analyser

type AstNodeRuleParams =
    { astNode : AstNode
      getParents : int -> AstNode list 
      fileContent : string }
    
type LineRuleParams =
    { line : string
      lineNumber : int
      fileContent : string }

type RuleMetadata<'config> =
  { name : string
    identifier : string option
    ruleConfig : 'config
  }

type AstNodeRuleConfig = { runner : AstNodeRuleParams -> LintSuggestion [] }

type LineRuleConfig<'Context> =
  { astFolder : ('Context -> AstNode -> 'Context) option
    runner : 'Context -> LineRuleParams -> LintSuggestion [] }
  
type IndentationRuleConfig = LineRuleConfig<Map<int,bool*int>>

type Rule =
  | AstNodeRule of RuleMetadata<AstNodeRuleConfig>
  | IndentationRule of RuleMetadata<IndentationRuleConfig>
