module FSharpLint.Framework.Rules

open FSharp.Compiler.Range
open FSharpLint.Framework.AbstractSyntaxArray
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Analyser

type AstNodeRuleParams =
    { astNode : AstNode
      nodeIndex : int
      syntaxArray : AbstractSyntaxArray.Node []
      skipArray : Skip []
      getParents : int -> AstNode list 
      fileContent : string }
    
type LineRuleParams =
    { line : string
      lineNumber : int
      isLastLine : bool
      fileContent : string }

type RuleMetadata<'config> =
  { name : string
    identifier : string option
    ruleConfig : 'config
  }

type AstNodeRuleConfig = { runner : AstNodeRuleParams -> LintSuggestion [] }
type LineRuleConfig = { runner : LineRuleParams -> LintSuggestion [] }

type LineRuleConfigWithContext<'Context> = { runner : 'Context -> LineRuleParams -> LintSuggestion [] }
  
type IndentationRuleConfig = LineRuleConfigWithContext<Map<int,bool*int>>
type NoTabCharactersRuleConfig = LineRuleConfigWithContext<(string*range) list>

type Rule =
  | AstNodeRule of RuleMetadata<AstNodeRuleConfig>
  | LineRule of RuleMetadata<LineRuleConfig>
  | IndentationRule of RuleMetadata<IndentationRuleConfig>
  | NoTabCharactersRule of RuleMetadata<NoTabCharactersRuleConfig>
