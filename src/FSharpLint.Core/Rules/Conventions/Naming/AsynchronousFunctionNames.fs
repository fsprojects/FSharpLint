module FSharpLint.Rules.AsynchronousFunctionNames

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open Helper.Naming.Asynchronous
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
    | AstNode.Binding (SynBinding (_, _, _, _, attributes, _, _, SynPat.LongIdent(funcIdent, _, _, _, (None | Some(SynAccess.Public _)), identRange), returnInfo, _, _, _, _))
        when not <| Helper.Naming.isAttribute "Obsolete" attributes ->
        let parents = args.GetParents args.NodeIndex
        let hasEnclosingFunctionOrMethod =
            parents
            |> List.exists
                (fun node ->
                    match node with
                    | AstNode.Binding (SynBinding (_, _, _, _, _, _, _, SynPat.LongIdent(_), _, _, _, _, _)) -> true
                    | _ -> false)
        
        if hasEnclosingFunctionOrMethod then
            Array.empty
        else
            match returnInfo with
            | Some ReturnsAsync ->
                match funcIdent with
                | HasAsyncPrefix _ ->
                    Array.empty
                | HasAsyncSuffix name 
                | HasNoAsyncPrefixOrSuffix name ->
                    let nameWithAsync = asyncSuffixOrPrefix + name
                    emitWarning identRange nameWithAsync "Async"
            | Some ReturnsTask ->
                match funcIdent with
                | HasAsyncSuffix _ ->
                    Array.empty
                | HasAsyncPrefix name 
                | HasNoAsyncPrefixOrSuffix name ->
                    let nameWithAsync = name + asyncSuffixOrPrefix
                    emitWarning identRange nameWithAsync "Task"
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
