module FSharpLint.Core.Tests.TestIndentationRuleBase

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.Rules

[<AbstractClass>]
type TestIndentationRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()

    override this.Parse (input:string, ?fileName:string, ?checkFile:bool, ?globalConfig:GlobalRuleConfig) =
        let checker = FSharpChecker.Create(keepAssemblyContents=true)
        let sourceText = SourceText.ofString input

        let fileName = Option.defaultValue "Test.fsx" fileName

        let projectOptions, _ = checker.GetProjectOptionsFromScript(fileName, sourceText) |> Async.RunSynchronously
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projectOptions
        let parseResults = checker.ParseFile("test.fsx", sourceText, parsingOptions) |> Async.RunSynchronously

        let rule =
            match rule with
            | IndentationRule rule -> rule
            | _ -> failwith "TestIndentationRuleBase only accepts IndentationRules"

        let globalConfig = Option.defaultValue GlobalRuleConfig.Default globalConfig

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
        let lineRules = { LineRules.IndentationRule = Some rule; NoTabCharactersRule = None; GenericLineRules = Array.empty }

        runLineRules
            {
                LineRules = lineRules
                GlobalConfig = globalConfig
                FilePath = fileName
                FileContent = input
                Lines = lines
                Context = context
            }
        |> Array.iter this.PostViolation
