module FSharpLint.Core.Tests.TestLineRuleBase

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.Rules

[<AbstractClass>]
type TestLineRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()

    override this.Parse (input:string, ?fileName:string, ?checkFile:bool, ?globalConfig:GlobalRuleConfig) =
        let checker = FSharpChecker.Create(keepAssemblyContents=true)
        let sourceText = SourceText.ofString input

        let fileName = Option.defaultValue "Test.fsx" fileName

        let projectOptions, _ = checker.GetProjectOptionsFromScript(fileName, sourceText) |> Async.RunSynchronously
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projectOptions
        let parseResults = checker.ParseFile("test.fsx", sourceText, parsingOptions) |> Async.RunSynchronously

        let globalConfig = Option.defaultValue GlobalRuleConfig.Default globalConfig

        let rule =
            match rule with
            | LineRule rule -> rule
            | _ -> failwith "TestLineRuleBase only accepts LineRules"

        let lines = input.Split "\n"

        let syntaxArray = AbstractSyntaxArray.astToArray parseResults.ParseTree
        let (_, context) =
            runAstNodeRules
                {
                    Rules = Array.empty
                    GlobalConfig = globalConfig
                    TypeCheckResults = None
                    FilePath = fileName
                    FileContent = input
                    Lines = lines
                    SyntaxArray = syntaxArray
                }
        let lineRules = { LineRules.IndentationRule = None; NoTabCharactersRule = None; GenericLineRules = [|rule|] }

        runLineRules
            {
                LineRules = lineRules
                GlobalConfig = globalConfig
                FilePath = fileName
                FileContent = input
                Lines = lines
                Context = context
            }
        |> Array.iter this.PostSuggestion