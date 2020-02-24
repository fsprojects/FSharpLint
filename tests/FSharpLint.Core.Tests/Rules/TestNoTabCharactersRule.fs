module TestNoTabCharactersRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.Rules

[<AbstractClass>]
type TestNoTabCharactersRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()

    override this.Parse (input:string, ?fileName:string, ?checkFile:bool) =
        let checker = FSharpChecker.Create()
        let sourceText = SourceText.ofString input

        let fileName = fileName |> Option.defaultValue "Test.fsx"

        let projectOptions, _ = checker.GetProjectOptionsFromScript(fileName, sourceText) |> Async.RunSynchronously
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projectOptions
        let parseResults = checker.ParseFile("test.fsx", sourceText, parsingOptions) |> Async.RunSynchronously

        let rule =
            match rule with
            | NoTabCharactersRule rule -> rule
            | _ -> failwithf "TestNoTabCharactersRuleBase only accepts NoTabCharactersRules"


        match parseResults.ParseTree with
        | Some tree ->
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray tree
            let (_, context) = runAstNodeRules Array.empty None fileName input syntaxArray skipArray
            let lineRules = { LineRules.indentationRule = None; noTabCharactersRule = Some rule; genericLineRules = [||] }

            runLineRules lineRules fileName input context
            |> Array.iter this.postSuggestion
        | None -> ()