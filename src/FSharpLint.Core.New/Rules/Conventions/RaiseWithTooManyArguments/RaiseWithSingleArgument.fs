module FSharpLint.Rules.RaiseWithSingleArgument

open FSharpLint.Framework.Rules
    
let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "raise" 1 "RulesRaiseWithSingleArgument"

let rule =
    { name = "RaiseWithSingleArgument" 
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner } }
    |> AstNodeRule
