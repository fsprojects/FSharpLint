module FSharpLint.Rules.UsedUnderscorePrefixedElements

open System

open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    // hack to only run rule once
    if args.NodeIndex = 0 then
        match args.CheckInfo with
        | Some checkResults -> 
            let allUsages = checkResults.GetAllUsesOfAllSymbolsInFile() |> Seq.toArray
            seq {
                for usage in allUsages do
                    match usage.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as symbol -> 
                        if not usage.IsFromDefinition && symbol.DisplayName.StartsWith "_" 
                                && symbol.DisplayName <> "_" && not symbol.IsCompilerGenerated && not symbol.IsMember then
                            let nameWithoutUnderscore = symbol.DisplayName.[1..]
                            let clashesWithOtherDefinitions =
                                allUsages
                                |> Array.exists 
                                    (fun each -> each.Symbol.DisplayName = nameWithoutUnderscore && each.IsFromDefinition)
                                
                            if clashesWithOtherDefinitions then
                                yield {
                                    Range = usage.Range
                                    Message = Resources.GetString ("RulesUsedUnderscorePrefixedElements")
                                    SuggestedFix = None
                                    TypeChecks = List.Empty
                                }
                            else
                                for range in [usage.Range; symbol.DeclarationLocation] do
                                    let warningDetrails = 
                                        lazy(
                                            let fromText = symbol.DisplayName
                                            Some { FromRange = range; FromText = fromText; ToText = nameWithoutUnderscore })
                                    yield {
                                        Range = range
                                        Message = Resources.GetString ("RulesUsedUnderscorePrefixedElements")
                                        SuggestedFix = Some warningDetrails
                                        TypeChecks = List.Empty
                                    }
                        else
                            ()
                    | _ -> () }
            |> Seq.distinctBy (fun wargningDetails -> wargningDetails.Range)
            |> Seq.toArray
        | None -> Array.empty
    else
        Array.empty

let rule =
    { Name = "UsedUnderscorePrefixedElements"
      Identifier = Identifiers.UsedUnderscorePrefixedElements
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
