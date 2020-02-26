module FSharpLint.Rules.MaxLinesInLambdaFunction

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.Expression(SynExpr.Lambda(_, _, _, _, range)) ->
        Helper.SourceLength.checkSourceLengthRule config range "Lambda function"
    | _ -> Array.empty

let rule config =
    { name = "MaxLinesInLambdaFunction"
      identifier = Identifiers.MaxLinesInLambdaFunction
      ruleConfig = { AstNodeRuleConfig.runner = runner config; cleanup = ignore } }
    |> AstNodeRule

let newRule (config:Helper.SourceLength.NewConfig) =
    rule { Helper.SourceLength.Config.maxLines = config.MaxLines }