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
        match args.CheckInfo with
        | Some checkResults -> 
            checkResults.GetAllUsesOfAllSymbolsInFile() 
            |> Seq.choose (fun usage ->
                match usage.Symbol with
                | :? FSharp.Compiler.Symbols.FSharpMemberOrFunctionOrValue as symbol -> 
                    if not usage.IsFromDefinition && symbol.FullName.StartsWith "_" 
                            && symbol.FullName <> "_" && not symbol.IsCompilerGenerated then
                        Some {
                            Range = usage.Range
                            Message = String.Format(Resources.GetString ("RulesUsedUnderscorePrefixedElements"))
                            SuggestedFix = None
                            TypeChecks = List.Empty
                        }
                    else
                        None
                | _ -> None )
            |> Seq.toArray
        | None -> Array.empty
    else
        Array.empty

let rule =
    { Name = "UsedUnderscorePrefixedElements"
      Identifier = Identifiers.UsedUnderscorePrefixedElements
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
