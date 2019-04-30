module FSharpLint.Rules.InvalidOpWithSingleArgument

open FSharpLint.Framework.Rules
    
let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "invalidOp" 1 "RulesInvalidOpWithSingleArgument"

let rule =
    { name = "InvalidOpWithSingleArgument" 
      identifier = Identifiers.InvalidOpWithSingleArgument
      ruleConfig = { AstNodeRuleConfig.runner = runner; cleanup = ignore } }
    |> AstNodeRule
