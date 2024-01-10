module FSharpLint.Rules.MaxLinesInMatchLambdaFunction

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.MatchLambda(_, _, _, _, range)) ->
        Helper.SourceLength.checkSourceLengthRule config range args.FileContent "Match lambda function"
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "MaxLinesInMatchLambdaFunction"
            Identifier = Identifiers.MaxLinesInMatchLambdaFunction
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
