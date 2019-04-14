module FSharpLint.Rules.FailwithWithSingleArgument

open FSharpLint.Framework.Rules
    
let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "failwith" 1 "FailwithWithSingleArgument" 

let rule =
    { name = "FailwithWithSingleArgument" 
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner } }
    |> AstNodeRule
