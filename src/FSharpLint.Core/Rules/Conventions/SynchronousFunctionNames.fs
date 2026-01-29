module FSharpLint.Rules.SynchronousFunctionNames

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols

let asyncSuffixOrPrefix = "Async"

let (|HasAsyncPrefix|HasAsyncSuffix|HasNoAsyncPrefixOrSuffix|) (pattern: SynLongIdent) =
    match List.tryLast pattern.LongIdent with
    | Some name ->
        if name.idText.StartsWith(asyncSuffixOrPrefix, StringComparison.InvariantCultureIgnoreCase) then
            HasAsyncPrefix name.idText
        elif name.idText.EndsWith asyncSuffixOrPrefix then
            HasAsyncSuffix name.idText
        else
            HasNoAsyncPrefixOrSuffix name.idText
    | _ -> HasNoAsyncPrefixOrSuffix String.Empty

let (|ReturnsTask|ReturnsAsync|ReturnsNonAsync|) (returnInfo: SynBindingReturnInfo) =
    match returnInfo with
    | SynBindingReturnInfo(SynType.LongIdent(SynLongIdent(typeIdent, _, _)), _, _, _)
    | SynBindingReturnInfo(SynType.App(SynType.LongIdent(SynLongIdent(typeIdent, _, _)), _, _, _, _, _, _), _, _, _) ->
        match List.tryLast typeIdent with
        | Some ident when ident.idText = "Async" -> ReturnsAsync
        | Some ident when ident.idText = "Task" -> ReturnsTask
        | _ -> ReturnsNonAsync
    | _ -> ReturnsNonAsync

let runner (args: AstNodeRuleParams) =
    let emitWarning range (newFunctionName: string) =
        Array.singleton
            {
                Range = range
                Message = String.Format(Resources.GetString "RulesSynchronousFunctionNames", newFunctionName)
                SuggestedFix = None
                TypeChecks = List.empty
            }

    match args.AstNode with
    | AstNode.Binding (SynBinding (_, _, _, _, _, _, _, SynPat.LongIdent(funcIdent, _, _, _, _, identRange), returnInfo, _, _, _, _)) ->
        match returnInfo with
        | Some ReturnsNonAsync ->
            match funcIdent with
            | HasAsyncPrefix name ->
                let nameWithoutAsync = name.Substring asyncSuffixOrPrefix.Length
                emitWarning identRange nameWithoutAsync
            | HasAsyncSuffix name ->
                let nameWithoutAsync = name.Substring(0, name.Length - asyncSuffixOrPrefix.Length)
                emitWarning identRange nameWithoutAsync
            | HasNoAsyncPrefixOrSuffix _ -> Array.empty
        | None -> 
            // TODO: get type using typed tree in args.CheckInfo
            Array.empty
        | _ ->
            Array.empty
    | _ -> Array.empty

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
