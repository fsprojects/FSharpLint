module FSharpLint.Rules.NullArgWithSingleArgument

open FSharpLint.Framework.Rules

let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "nullArg" 1 "RulesNullArgWithSingleArgument"

let rule =
    { name = "NullArgWithSingleArgument"
      identifier = Identifiers.NullArgWithSingleArgument
      ruleConfig = { AstNodeRuleConfig.runner = runner; cleanup = ignore } }
    |> AstNodeRule
