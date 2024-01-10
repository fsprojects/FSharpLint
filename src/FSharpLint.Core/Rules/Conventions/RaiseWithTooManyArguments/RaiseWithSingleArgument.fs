module FSharpLint.Rules.RaiseWithSingleArgument

open FSharpLint.Framework.Rules

let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "raise" 1 "RulesRaiseWithSingleArgument"

let rule =
    AstNodeRule
        {
            Name = "RaiseWithSingleArgument"
            Identifier = Identifiers.RaiseWithSingleArgument
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
