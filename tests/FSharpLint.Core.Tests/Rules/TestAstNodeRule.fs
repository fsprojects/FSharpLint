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

    override this.Parse (input:string, ?maybeFileName:string, ?checkFile:bool, ?globalConfig:GlobalRuleConfig) =
        let checker = FSharpChecker.Create(keepAssemblyContents=true)

        let parseResults =
            match maybeFileName with
            | Some fileName ->
                ParseFile.parseSourceFile fileName input checker
            | None ->
                ParseFile.parseSource input checker

        let astNodeRule =
            match rule with
            | AstNodeRule nodeRule -> nodeRule
            | _ -> failwith "TestAstNodeRuleBase only accepts AstNodeRules"

        let resolvedGlobalConfig = Option.defaultValue GlobalRuleConfig.Default globalConfig

        match Async.RunSynchronously parseResults with
        | ParseFileResult.Success parseInfo ->
            let syntaxArray = AbstractSyntaxArray.astToArray parseInfo.Ast
            let checkResult =
                match checkFile with
                | Some false -> None
                | _ -> parseInfo.TypeCheckResults

            let suggestions =
                runAstNodeRules
                    {
                        Rules = Array.singleton astNodeRule
                        GlobalConfig = resolvedGlobalConfig
                        TypeCheckResults = checkResult
                        ProjectCheckResults = None
                        ProjectOptions = Lazy<_>(None)
                        FilePath = (Option.defaultValue String.Empty maybeFileName)
                        FileContent = input
                        Lines = (input.Split("\n"))
                        SyntaxArray = syntaxArray
                    }
                |> fst

            astNodeRule.RuleConfig.Cleanup()

            Array.iter this.PostSuggestion suggestions
        | _ ->
            failwith "Failed to parse"
