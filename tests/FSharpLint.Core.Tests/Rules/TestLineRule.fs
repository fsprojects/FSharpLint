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

    override this.Parse (input:string, ?fileName:string, ?_checkFile:bool, ?globalConfig:GlobalRuleConfig) =
        let checker = FSharpChecker.Create(keepAssemblyContents=true)
        let sourceText = SourceText.ofString input

        let resolvedFileName = Option.defaultValue "Test.fsx" fileName

        let projectOptions, _ = checker.GetProjectOptionsFromScript(resolvedFileName, sourceText) |> Async.RunSynchronously
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projectOptions
        let parseResults = checker.ParseFile("test.fsx", sourceText, parsingOptions) |> Async.RunSynchronously

        let resolvedGlobalConfig = Option.defaultValue GlobalRuleConfig.Default globalConfig

        let lineRule =
            match rule with
            | LineRule lRule -> lRule
            | _ -> failwith "TestLineRuleBase only accepts LineRules"

        let lines = input.Split "\n"

        let syntaxArray = AbstractSyntaxArray.astToArray parseResults.ParseTree
        let (_, context) =
            runAstNodeRules
                {
                    Rules = Array.empty
                    GlobalConfig = resolvedGlobalConfig
                    TypeCheckResults = None
                    ProjectCheckResults = None
                    FilePath = resolvedFileName
                    FileContent = input
                    Lines = lines
                    SyntaxArray = syntaxArray
                }
        let lineRules = { LineRules.IndentationRule = None; NoTabCharactersRule = None; GenericLineRules = [|lineRule|] }

        runLineRules
            {
                LineRules = lineRules
                GlobalConfig = resolvedGlobalConfig
                FilePath = resolvedFileName
                FileContent = input
                Lines = lines
                Context = context
            }
        |> Array.iter this.PostSuggestion