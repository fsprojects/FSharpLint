﻿module TestLineRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.Rules

[<AbstractClass>]
type TestLineRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()

    override this.Parse (input:string, ?fileName:string, ?checkFile:bool, ?globalConfig : GlobalRuleConfig) =
        let checker = FSharpChecker.Create()
        let sourceText = SourceText.ofString input

        let fileName = fileName |> Option.defaultValue "Test.fsx"

        let projectOptions, _ = checker.GetProjectOptionsFromScript(fileName, sourceText) |> Async.RunSynchronously
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projectOptions
        let parseResults = checker.ParseFile("test.fsx", sourceText, parsingOptions) |> Async.RunSynchronously

        let globalConfig = globalConfig |> Option.defaultValue GlobalRuleConfig.Default

        let rule =
            match rule with
            | LineRule rule -> rule
            | _ -> failwithf "TestLineRuleBase only accepts LineRules"


        match parseResults.ParseTree with
        | Some tree ->
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray tree
            let (_, context) = runAstNodeRules Array.empty globalConfig None fileName input syntaxArray skipArray
            let lineRules = { LineRules.IndentationRule = None; noTabCharactersRule = None; genericLineRules = [|rule|] }

            runLineRules lineRules globalConfig fileName input context
            |> Array.iter this.postSuggestion
        | None -> ()