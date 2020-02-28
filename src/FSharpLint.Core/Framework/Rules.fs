module FSharpLint.Framework.Rules

open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices
open FSharpLint.Framework.AbstractSyntaxArray
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Suggestion

type GlobalRuleConfig =
    {
        numIndentationSpaces : int
    }
with
    static member Default =
        {
            GlobalRuleConfig.numIndentationSpaces = 4
        }

type AstNodeRuleParams =
    { AstNode : AstNode
      NodeHashcode : int
      NodeIndex : int
      SyntaxArray : AbstractSyntaxArray.Node []
      SkipArray : Skip []
      GetParents : int -> AstNode list
      FilePath : string
      FileContent : string
      CheckInfo : FSharpCheckFileResults option
      GlobalConfig : GlobalRuleConfig }

type LineRuleParams =
    { Line : string
      LineNumber : int
      IsLastLine : bool
      FilePath : string
      FileContent : string
      GlobalConfig : GlobalRuleConfig }

type RuleMetadata<'config> =
  { Name : string
    Identifier : string
    RuleConfig : 'config }

type AstNodeRuleConfig =
  { Runner : AstNodeRuleParams -> WarningDetails []
    Cleanup : unit -> unit }

type NamingCase =
    | PascalCase = 0
    | CamelCase = 1

type NamingUnderscores =
    | None = 0
    | AllowPrefix = 1
    | AllowAny = 2

type NamingConfig =
    { Naming : NamingCase option
      Underscores : NamingUnderscores option
      Prefix : string option
      Suffix : string option }

type NamingRuleConfig =
    { Config : NamingConfig
      GetIdentifiersToCheck : AstNodeRuleParams -> (Ident * string * Async<bool> option) [] }

type LineRuleConfig = { Runner : LineRuleParams -> WarningDetails [] }

type LineRuleConfigWithContext<'Context> = { Runner : 'Context -> LineRuleParams -> WarningDetails [] }

type IndentationRuleConfig = LineRuleConfigWithContext<Map<int,bool*int>>
type NoTabCharactersRuleConfig = LineRuleConfigWithContext<(string*range) list>

type Rule =
    | AstNodeRule of RuleMetadata<AstNodeRuleConfig>
    | LineRule of RuleMetadata<LineRuleConfig>
    | IndentationRule of RuleMetadata<IndentationRuleConfig>
    | NoTabCharactersRule of RuleMetadata<NoTabCharactersRuleConfig>

let toWarning (identifier:string) (ruleName:string) (filePath:string) (fileContents:string) (details:WarningDetails) =
    {
        LintWarning.RuleIdentifier = identifier
        FilePath = filePath
        RuleName = ruleName
        ErrorText = fileContents.Split('\n').[details.Range.StartLine - 1].TrimEnd('\r')
        Details = details
    }

let runAstNodeRule (rule:RuleMetadata<AstNodeRuleConfig>) (config:AstNodeRuleParams) =
    rule.RuleConfig.Runner config
    |> Array.map (toWarning rule.Identifier rule.Name config.FilePath config.FileContent)

let runLineRuleWithContext (rule:RuleMetadata<LineRuleConfigWithContext<'Context>>) (context:'Context) (config:LineRuleParams) =
    rule.RuleConfig.Runner context config
    |> Array.map (toWarning rule.Identifier rule.Name config.FilePath config.FileContent)

let runLineRule (rule:RuleMetadata<LineRuleConfig>) (config:LineRuleParams) =
    rule.RuleConfig.Runner config
    |> Array.map (toWarning rule.Identifier rule.Name config.FilePath config.FileContent)
