module FSharpLint.Rules.NoImpureFunctions

open System
open FSharpLint.Framework.Rules

[<RequireQualifiedAccess>]
type Config = {
    AllowedImpureFunctions:string list
    AdditionalImpureFunctions:string list
}

let runner (_config:Config) (_args:AstNodeRuleParams) =
    failwith "Not yet implemented"

let rule config =
    AstNodeRule
        {
            Name = "NoImpureFunctions"
            Identifier = Identifiers.NoImpureFunctions
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }

