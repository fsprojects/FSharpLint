module FSharpLint.Rules.MaxNumberOfFunctionParameters

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private validateFunction (maxParameters:int) (constructorArguments:SynArgPats) =
    match constructorArguments with
    | SynArgPats.Pats(parameters)
            when List.length parameters > maxParameters ->
        let errorFormatString = Resources.GetString("RulesNumberOfItemsFunctionError")
        let error = String.Format(errorFormatString, maxParameters)
        { Range = parameters.[maxParameters].Range; Message = error; SuggestedFix = None; TypeChecks = [] } |> Array.singleton
    | _ -> Array.empty

let private runner (config:Helper.NumberOfItems.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Pattern(SynPat.LongIdent(_, _, _, constructorArguments, _, _)) ->
        validateFunction config.MaxItems constructorArguments
    | _ -> Array.empty

let rule config =
    { Name = "MaxNumberOfFunctionParameters"
      Identifier = Identifiers.MaxNumberOfFunctionParameters
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule
