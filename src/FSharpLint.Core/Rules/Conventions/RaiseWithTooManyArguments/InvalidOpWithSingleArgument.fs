module internal FSharpLint.Rules.InvalidOpWithSingleArgument

open FSharpLint.Framework.Rules

let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "invalidOp" 1 "RulesInvalidOpWithSingleArgument"

let rule =
    { Name = "InvalidOpWithSingleArgument"
      Identifier = Identifiers.InvalidOpWithSingleArgument
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
