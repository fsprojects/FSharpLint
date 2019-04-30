module FSharpLint.Rules.InvalidArgWithTwoArguments

open FSharpLint.Framework.Rules
    
let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "invalidArg" 2 "InvalidArgWithTwoArguments"

let rule =
    { name = "InvalidArgWithTwoArguments" 
      identifier = Identifiers.InvalidOpWithSingleArgument
      ruleConfig = { AstNodeRuleConfig.runner = runner; cleanup = ignore } }
    |> AstNodeRule
