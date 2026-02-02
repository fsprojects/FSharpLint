module FSharpLint.Rules.SynchronousFunctionNames

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open Helper.Naming.Asynchronous
open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols

let asyncSuffixOrPrefix = "Async"

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
    | AstNode.Binding (SynBinding (_, _, _, _, attributes, _, _, SynPat.LongIdent(funcIdent, _, _, _, _, identRange), returnInfo, _, _, _, _))
        when not <| Helper.Naming.isAttribute "Obsolete" attributes ->
        match returnInfo with
        | Some ReturnsNonAsync ->
            match funcIdent with
            | HasAsyncPrefix name ->
                let startsWithLowercase = Char.IsLower name.[0]
                let nameWithoutAsync = name.Substring asyncSuffixOrPrefix.Length
                let suggestedName = 
                    if startsWithLowercase then
                        sprintf "%c%s" (Char.ToLowerInvariant nameWithoutAsync.[0]) (nameWithoutAsync.Substring 1)
                    else
                        nameWithoutAsync
                emitWarning identRange suggestedName
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
