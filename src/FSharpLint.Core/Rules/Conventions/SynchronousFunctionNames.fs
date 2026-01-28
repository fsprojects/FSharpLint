module FSharpLint.Rules.SynchronousFunctionNames

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    failwith "Not yet implemented"

let rule =
    AstNodeRule
        {
            Name = "SynchronousFunctionNames"
            Identifier = Identifiers.SynchronousFunctionNames
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
