module TestAstNodeRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.ParseFile
open FSharpLint.Framework.Rules

[<AbstractClass>]
type internal TestAstNodeRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()

    override this.Parse (input:string, ?fileName:string, ?checkFile:bool, ?globalConfig:GlobalRuleConfig) =
        let checker = FSharpChecker.Create()

        let parseResults =
            match fileName with
            | Some fileName ->
                ParseFile.parseSource input checker
            | None ->
                ParseFile.parseSource input checker

        let rule =
            match rule with
            | AstNodeRule rule -> rule
            | _ -> failwithf "TestAstNodeRuleBase only accepts AstNodeRules"

        let globalConfig = globalConfig |> Option.defaultValue GlobalRuleConfig.Default

        match parseResults with
        | Ok parseInfo ->
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray parseInfo.Ast
            let checkResult =
                match checkFile with
                | Some false -> None
                | _ -> parseInfo.TypeCheckResults
            let suggestions = LintRunner.runAstNodeRules (Array.singleton rule) globalConfig checkResult (Option.defaultValue "" fileName) input (input.Split("\n")) syntaxArray skipArray |> fst
            rule.RuleConfig.Cleanup()

            suggestions |> Array.iter this.PostSuggestion
        | Error e ->
            failwithf "Failed to parse, %O" e