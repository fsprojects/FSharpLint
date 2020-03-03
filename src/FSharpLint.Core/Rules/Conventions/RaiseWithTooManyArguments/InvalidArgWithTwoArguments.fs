module FSharpLint.Rules.InvalidArgWithTwoArguments

open FSharpLint.Framework.Rules

let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "invalidArg" 2 "InvalidArgWithTwoArguments"

let rule =
    { Name = "InvalidArgWithTwoArguments"
      Identifier = Identifiers.InvalidOpWithSingleArgument
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
