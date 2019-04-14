module FSharpLint.Rules.InvalidArgWithTwoArguments

open FSharpLint.Framework.Rules
    
let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "invalidArg" 2 "InvalidArgWithTwoArguments"

let rule =
    { name = "InvalidArgWithTwoArguments" 
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner } }
    |> AstNodeRule
