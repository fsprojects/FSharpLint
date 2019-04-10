module FSharpLint.Rules.MaxNumberOfFunctionParameters

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<RequireQualifiedAccess>]
type Config =
    { maxParameters : int }

let private validateFunction (maxParameters:int) (constructorArguments:SynConstructorArgs) =
    match constructorArguments with
    | SynConstructorArgs.Pats(parameters)
            when List.length parameters > maxParameters ->
        let errorFormatString = Resources.GetString("RulesNumberOfItemsFunctionError")
        let error = String.Format(errorFormatString, maxParameters)
        { Range = parameters.[maxParameters].Range; Message = error; SuggestedFix = None; TypeChecks = [] } |> Array.singleton
    | _ -> Array.empty
    
let private runner (config:Config) (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.Pattern(SynPat.LongIdent(_, _, _, constructorArguments, _, _)) ->
        validateFunction config.maxParameters constructorArguments
    | _ -> Array.empty
    
let rule config =
    { name = "MaxNumberOfFunctionParameters"
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner config } }
    |> AstNodeRule
