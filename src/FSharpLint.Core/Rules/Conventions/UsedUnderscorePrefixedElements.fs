module FSharpLint.Rules.UsedUnderscorePrefixedElements

open System

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
        let processSymbolUse (usage: FSharpSymbolUse) =
            match usage.Symbol with
            | :? FSharp.Compiler.Symbols.FSharpMemberOrFunctionOrValue as symbol -> 
                let conditions =
                    not usage.IsFromDefinition
                    && symbol.FullName.StartsWith "_"
                    && symbol.FullName <> "_"
                    && not symbol.IsCompilerGenerated
                if conditions then
                    Some {
                        Range = usage.Range
                        Message = String.Format(Resources.GetString ("RulesUsedUnderscorePrefixedElements"))
                        SuggestedFix = None
                        TypeChecks = List.Empty
                    }
                else
                    None
            | _ -> None

        match args.CheckInfo with
        | Some checkResults -> 
            checkResults.GetAllUsesOfAllSymbolsInFile() 
            |> Seq.choose processSymbolUse
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
