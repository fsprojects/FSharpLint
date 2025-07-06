﻿module FSharpLint.Core.Tests.TestNoTabCharactersRuleBase

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.Rules

[<AbstractClass>]
type TestNoTabCharactersRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()

    override this.Parse (input:string, ?fileName:string, ?checkFile:bool, ?globalConfig:GlobalRuleConfig) =
        let checker = FSharpChecker.Create(keepAssemblyContents=true)
        let sourceText = SourceText.ofString input

        let fileName = fileName |> Option.defaultValue "Test.fsx"

        let projectOptions, _ = checker.GetProjectOptionsFromScript(fileName, sourceText) |> Async.RunSynchronously
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projectOptions
        let parseResults = checker.ParseFile("test.fsx", sourceText, parsingOptions) |> Async.RunSynchronously

        let rule =
            match rule with
            | NoTabCharactersRule rule -> rule
            | _ -> failwithf "TestNoTabCharactersRuleBase only accepts NoTabCharactersRules"

        let globalConfig = globalConfig |> Option.defaultValue GlobalRuleConfig.Default

        let lines = input.Split "\n"

        let syntaxArray = AbstractSyntaxArray.astToArray parseResults.ParseTree
        let (_, context) = runAstNodeRules Array.empty globalConfig None fileName input lines syntaxArray
        let lineRules = { LineRules.IndentationRule = None; NoTabCharactersRule = Some rule; GenericLineRules = [||] }

        runLineRules lineRules globalConfig fileName input lines context
        |> Array.iter this.PostSuggestion