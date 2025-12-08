module FSharpLint.Rules.MaxNumberOfFunctionParameters

open System
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private validateFunction (maxParameters:int) (constructorArguments:SynArgPats) =
    match constructorArguments with
    | SynArgPats.Pats(parameters)
            when List.length parameters > maxParameters ->
        let violationTextFormatString = Resources.GetString "RulesNumberOfItemsFunctionViolation"
        let violationMsg = String.Format(violationTextFormatString, maxParameters)
        Array.singleton
            {
                Range = parameters.[maxParameters].Range
                Message = violationMsg
                SuggestedFix = None
                TypeChecks = List.Empty
            }
    | _ -> Array.empty

let private runner (config:Helper.NumberOfItems.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Pattern(SynPat.LongIdent(_, _, _, constructorArguments, _, _)) ->
        validateFunction config.MaxItems constructorArguments
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "MaxNumberOfFunctionParameters"
            Identifier = Identifiers.MaxNumberOfFunctionParameters
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
