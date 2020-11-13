module FSharpLint.Rules.MaxLinesInLambdaFunction

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.Lambda(_, _, _, _, _, range)) ->
        Helper.SourceLength.checkSourceLengthRule config range "Lambda function"
    | _ -> Array.empty

let rule config =
    { Name = "MaxLinesInLambdaFunction"
      Identifier = Identifiers.MaxLinesInLambdaFunction
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule