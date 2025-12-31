module FSharpLint.Rules.UsedUnderscorePrefixedElements

open System

open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    // hack to only run rule once
    if args.NodeIndex = 0 then
        let processSymbolUse (allUsages: seq<FSharpSymbolUse>) (usage: FSharpSymbolUse) = seq {
            match usage.Symbol with
            | :? FSharpMemberOrFunctionOrValue as symbol -> 
                let conditions =
                    not usage.IsFromDefinition
                    && symbol.FullName.StartsWith "_"
                    && symbol.FullName <> "_"
                    && not symbol.IsCompilerGenerated
                if conditions then
                    let nameWithoutUnderscore = symbol.DisplayName.[1..]
                    let clashesWithOtherDefinitions =
                        allUsages
                        |> Seq.exists 
                            (fun each -> each.Symbol.DisplayName = nameWithoutUnderscore && each.IsFromDefinition)
                                
                    if clashesWithOtherDefinitions then
                        yield  {
                            Range = usage.Range
                            Message = String.Format(Resources.GetString ("RulesUsedUnderscorePrefixedElements"))
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
                                Message = String.Format(Resources.GetString ("RulesUsedUnderscorePrefixedElements"))
                                SuggestedFix = Some warningDetrails
                                TypeChecks = List.Empty
                            }
                else
                    ()
            | _ -> ()
        }

        match args.CheckInfo with
        | Some checkResults -> 
            let allUsages = checkResults.GetAllUsesOfAllSymbolsInFile() 
            allUsages
            |> Seq.collect (processSymbolUse allUsages)
            |> Seq.distinctBy (fun wargningDetails -> wargningDetails.Range)
            |> Seq.toArray
        | None -> Array.empty
    else
        Array.empty

let rule =
    AstNodeRule
        {
            Name = "UsedUnderscorePrefixedElements"
            Identifier = Identifiers.UsedUnderscorePrefixedElements
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
