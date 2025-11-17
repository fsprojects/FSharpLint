module FSharpLint.Rules.NullArgWithSingleArgument

open FSharpLint.Framework.Rules

let runner = Helper.RaiseWithTooManyArguments.checkRaiseWithTooManyArgs "nullArg" 1 "RulesNullArgWithSingleArgument"

let rule =
    AstNodeRule
        {
            Name = "NullArgWithSingleArgument"
            Identifier = Identifiers.NullArgWithSingleArgument
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
