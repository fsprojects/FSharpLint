﻿module FSharpLint.Framework.Rules

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices
open FSharpLint.Framework.AbstractSyntaxArray
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Suggestion

// Non-standard record field names for serialization
// fsharplint:disable RecordFieldNames
type GlobalRuleConfig = {
    numIndentationSpaces : int
} with
    static member Default = {
        GlobalRuleConfig.numIndentationSpaces = 4
    }
// fsharplint:enable RecordFieldNames

type internal AstNodeRuleParams =
    { AstNode : AstNode
      NodeHashcode : int
      NodeIndex : int
      SyntaxArray : AbstractSyntaxArray.Node []
      SkipArray : Skip []
      GetParents : int -> AstNode list
      FilePath : string
      FileContent : string
      Lines : string []
      CheckInfo : FSharpCheckFileResults option
      GlobalConfig : GlobalRuleConfig }

type internal LineRuleParams =
    { Line : string
      LineNumber : int
      IsLastLine : bool
      FilePath : string
      FileContent : string
      Lines : string []
      GlobalConfig : GlobalRuleConfig }

type internal RuleMetadata<'config> =
  { Name : string
    Identifier : string
    RuleConfig : 'config }

type internal AstNodeRuleConfig =
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

type internal NamingRuleConfig =
    { Config : NamingConfig
      GetIdentifiersToCheck : AstNodeRuleParams -> (Ident * string * Async<bool> option) [] }

type internal LineRuleConfig = { Runner : LineRuleParams -> WarningDetails [] }

type internal LineRuleConfigWithContext<'Context> = { Runner : 'Context -> LineRuleParams -> WarningDetails [] }

type internal IndentationRuleConfig = LineRuleConfigWithContext<Map<int,bool*int>>
type internal NoTabCharactersRuleConfig = LineRuleConfigWithContext<(string*range) list>

type internal Rule =
    | AstNodeRule of RuleMetadata<AstNodeRuleConfig>
    | LineRule of RuleMetadata<LineRuleConfig>
    | IndentationRule of RuleMetadata<IndentationRuleConfig>
    | NoTabCharactersRule of RuleMetadata<NoTabCharactersRuleConfig>

let internal toWarning (identifier:string) (ruleName:string) (filePath:string) (lines:string []) (details:WarningDetails) =
    {
        LintWarning.RuleIdentifier = identifier
        FilePath = filePath
        RuleName = ruleName
        ErrorText = lines.[details.Range.StartLine - 1].TrimEnd('\r')
        Details = details
    }

let internal runAstNodeRule (rule:RuleMetadata<AstNodeRuleConfig>) (config:AstNodeRuleParams) =
    rule.RuleConfig.Runner config
    |> Array.map (toWarning rule.Identifier rule.Name config.FilePath config.Lines)

let internal runLineRuleWithContext (rule:RuleMetadata<LineRuleConfigWithContext<'Context>>) (context:'Context) (config:LineRuleParams) =
    rule.RuleConfig.Runner context config
    |> Array.map (toWarning rule.Identifier rule.Name config.FilePath config.Lines)

let internal runLineRule (rule:RuleMetadata<LineRuleConfig>) (config:LineRuleParams) =
    rule.RuleConfig.Runner config
    |> Array.map (toWarning rule.Identifier rule.Name config.FilePath config.Lines)
