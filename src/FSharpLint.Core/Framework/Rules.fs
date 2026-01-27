module FSharpLint.Framework.Rules

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework.AbstractSyntaxArray
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Suggestion

// Non-standard record field names for serialization
// fsharplint:disable RecordFieldNames
type GlobalRuleConfig =
    {
        numIndentationSpaces:int
    }
with
    static member Default =
        {
            GlobalRuleConfig.numIndentationSpaces = 4
        }
// fsharplint:enable RecordFieldNames

type AstNodeRuleParams =
    { AstNode:AstNode
      NodeHashcode:int
      NodeIndex:int
      SyntaxArray:AbstractSyntaxArray.Node []
      GetParents:int -> AstNode list
      FilePath:string
      FileContent:string
      Lines:string []
      CheckInfo:FSharpCheckFileResults option
      ProjectCheckInfo:FSharpCheckProjectResults option
      GlobalConfig:GlobalRuleConfig }

type LineRuleParams =
    { Line:string
      LineNumber:int
      IsLastLine:bool
      FilePath:string
      FileContent:string
      Lines:string []
      GlobalConfig:GlobalRuleConfig }

type RuleMetadata<'Config> =
    { Name:string
      Identifier:string
      RuleConfig:'Config }

type AstNodeRuleConfig =
    { Runner:AstNodeRuleParams -> WarningDetails []
      Cleanup:unit -> unit }

type NamingCase =
    | PascalCase = 0
    | CamelCase = 1
    | AllLowercase = 2
    | AllUppercase = 3

type NamingUnderscores =
    | None = 0
    | AllowPrefix = 1
    | AllowAny = 2
    | AllowInfix = 3

type NamingConfig =
    { Naming:NamingCase option
      Underscores:NamingUnderscores option
      Prefix:string option
      Suffix:string option }

type NamingRuleConfig =
    { Config:NamingConfig
      GetIdentifiersToCheck:AstNodeRuleParams -> (Ident * string * (unit -> bool) option) [] }

type LineRuleConfig = { Runner:LineRuleParams -> WarningDetails [] }

type LineRuleConfigWithContext<'Context> = { Runner:'Context -> LineRuleParams -> WarningDetails [] }

type IndentationRuleConfig = LineRuleConfigWithContext<Map<int,bool*int>>
type NoTabCharactersRuleConfig = LineRuleConfigWithContext<(string*range) list>

type Rule =
    | AstNodeRule of rule: RuleMetadata<AstNodeRuleConfig>
    | LineRule of rule: RuleMetadata<LineRuleConfig>
    | IndentationRule of rule: RuleMetadata<IndentationRuleConfig>
    | NoTabCharactersRule of rule: RuleMetadata<NoTabCharactersRuleConfig>

let toWarning (identifier:string) (ruleName:string) (filePath:string) (lines:string []) (details:WarningDetails) =
    {
        LintWarning.RuleIdentifier = identifier
        FilePath = filePath
        RuleName = ruleName
        ErrorText = lines.[details.Range.StartLine - 1].TrimEnd('\r')
        Details = details
    }

let runAstNodeRule (rule:RuleMetadata<AstNodeRuleConfig>) (config:AstNodeRuleParams) =
    rule.RuleConfig.Runner config
    |> Array.map (toWarning rule.Identifier rule.Name config.FilePath config.Lines)

let runLineRuleWithContext (rule:RuleMetadata<LineRuleConfigWithContext<'Context>>) (context:'Context) (config:LineRuleParams) =
    rule.RuleConfig.Runner context config
    |> Array.map (toWarning rule.Identifier rule.Name config.FilePath config.Lines)

let runLineRule (rule:RuleMetadata<LineRuleConfig>) (config:LineRuleParams) =
    rule.RuleConfig.Runner config
    |> Array.map (toWarning rule.Identifier rule.Name config.FilePath config.Lines)
