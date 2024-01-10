module FSharpLint.Core.Tests.TestAstNodeRuleBase

open System
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.ParseFile
open FSharpLint.Framework.Rules

[<AbstractClass>]
type TestAstNodeRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()

    override this.Parse (input:string, ?fileName:string, ?checkFile:bool, ?globalConfig:GlobalRuleConfig) =
        let checker = FSharpChecker.Create(keepAssemblyContents=true)

        let parseResults =
            match fileName with
            | Some fileName ->
                ParseFile.parseSourceFile fileName input checker
            | None ->
                ParseFile.parseSource input checker

        let rule =
            match rule with
            | AstNodeRule rule -> rule
            | _ -> failwithf "TestAstNodeRuleBase only accepts AstNodeRules"

        let globalConfig = Option.defaultValue GlobalRuleConfig.Default globalConfig

        match parseResults with
        | ParseFileResult.Success parseInfo ->
            let syntaxArray = AbstractSyntaxArray.astToArray parseInfo.Ast
            let checkResult =
                match checkFile with
                | Some false -> None
                | _ -> parseInfo.TypeCheckResults

            let suggestions =
                runAstNodeRules
                    {
                        Rules = Array.singleton rule
                        GlobalConfig = globalConfig
                        TypeCheckResults = checkResult
                        FilePath = (Option.defaultValue String.Empty fileName)
                        FileContent = input
                        Lines = (input.Split("\n"))
                        SyntaxArray = syntaxArray
                    }
                |> fst

            rule.RuleConfig.Cleanup()

            Array.iter this.PostSuggestion suggestions
        | _ ->
            failwithf "Failed to parse"