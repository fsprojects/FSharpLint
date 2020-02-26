module FSharpLint.Rules.FailwithWithSingleArgument

open FSharpLint.Framework.Rules

let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "failwith" 1 "FailwithWithSingleArgument"

let rule =
    { name = "FailwithWithSingleArgument"
      identifier = Identifiers.FailwithWithSingleArgument
      ruleConfig = { AstNodeRuleConfig.runner = runner; cleanup = ignore } }
    |> AstNodeRule
