module FSharpLint.Rules.FailwithWithSingleArgument

open FSharpLint.Framework.Rules

let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "failwith" 1 "FailwithWithSingleArgument"

let rule =
    AstNodeRule
        {
            Name = "FailwithWithSingleArgument"
            Identifier = Identifiers.FailwithWithSingleArgument
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
