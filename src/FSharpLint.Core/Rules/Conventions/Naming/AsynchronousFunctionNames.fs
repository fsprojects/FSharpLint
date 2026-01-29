module FSharpLint.Rules.AsynchronousFunctionNames

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharp.Compiler.Syntax

let runner (args: AstNodeRuleParams) =
    let emitWarning range (newFunctionName: string) (returnTypeName: string) =
        Array.singleton
            {
                Range = range
                Message = String.Format(Resources.GetString "RulesAsynchronousFunctionNames", returnTypeName, newFunctionName)
                SuggestedFix = None
                TypeChecks = List.empty
            }
    
    match args.AstNode with
    | AstNode.Binding (SynBinding (_, _, _, _, _, _, _, SynPat.LongIdent(funcIdent, _, _, _, _, identRange), returnInfo, _, _, _, _)) ->
        match returnInfo with
        | Some SynchronousFunctionNames.ReturnsAsync ->
            match funcIdent with
            | SynchronousFunctionNames.HasAsyncPrefix _ ->
                Array.empty
            | SynchronousFunctionNames.HasAsyncSuffix name 
            | SynchronousFunctionNames.HasNoAsyncPrefixOrSuffix name ->
                let nameWithAsync = SynchronousFunctionNames.asyncSuffixOrPrefix + name
                emitWarning identRange nameWithAsync "Async"
        | None -> 
            // TODO: get type using typed tree in args.CheckInfo
            Array.empty
        | _ ->
            Array.empty
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "AsynchronousFunctionNames"
            Identifier = Identifiers.AsynchronousFunctionNames
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
