module FSharpLint.Rules.InvalidOpWithSingleArgument

open FSharpLint.Framework.Rules
    
let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "invalidOp" 1 "RulesInvalidOpWithSingleArgument"

let rule =
    { name = "InvalidOpWithSingleArgument" 
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner } }
    |> AstNodeRule
