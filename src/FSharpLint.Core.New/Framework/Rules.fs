module FSharpLint.Framework.Rules

open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices
open FSharpLint.Framework.AbstractSyntaxArray
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Suggestion

type AstNodeRuleParams =
    { astNode : AstNode
      nodeIndex : int
      syntaxArray : AbstractSyntaxArray.Node []
      skipArray : Skip []
      getParents : int -> AstNode list 
      fileContent : string
      checkInfo : FSharpCheckFileResults option }
    
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

type AstNodeRuleConfig =
  { runner : AstNodeRuleParams -> LintSuggestion []
    cleanup : unit -> unit }
  
type NamingCase =
    | PascalCase = 0
    | CamelCase = 1

type NamingUnderscores =
    | None = 0
    | AllowPrefix = 1

type NamingConfig =
    { naming : NamingCase option
      underscores : NamingUnderscores option
      prefix : string option
      suffix : string option }
    
type NamingRuleConfig =
    { config : NamingConfig
      getIdentifiersToCheck : AstNodeRuleParams -> (Ident * string * Async<bool> option) [] }
    
type LineRuleConfig = { runner : LineRuleParams -> LintSuggestion [] }

type LineRuleConfigWithContext<'Context> = { runner : 'Context -> LineRuleParams -> LintSuggestion [] }
  
type IndentationRuleConfig = LineRuleConfigWithContext<Map<int,bool*int>>
type NoTabCharactersRuleConfig = LineRuleConfigWithContext<(string*range) list>

type Rule =
  | AstNodeRule of RuleMetadata<AstNodeRuleConfig>
  | LineRule of RuleMetadata<LineRuleConfig>
  | IndentationRule of RuleMetadata<IndentationRuleConfig>
  | NoTabCharactersRule of RuleMetadata<NoTabCharactersRuleConfig>
