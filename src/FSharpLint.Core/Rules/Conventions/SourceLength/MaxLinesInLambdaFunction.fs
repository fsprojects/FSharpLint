module FSharpLint.Rules.MaxLinesInLambdaFunction

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.Lambda(_, _, _, _, _, range, _)) ->
        Helper.SourceLength.checkSourceLengthRule config range args.FileContent "Lambda function"
    | _ -> Array.empty

let rule config =
    { Name = "MaxLinesInLambdaFunction"
      Identifier = Identifiers.MaxLinesInLambdaFunction
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule