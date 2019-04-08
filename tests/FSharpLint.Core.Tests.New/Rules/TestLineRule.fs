module TestLineRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Rules

[<AbstractClass>]
type TestLineRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()
    
    override this.Parse (input:string) =
        let checker = FSharpChecker.Create()
        
        let projectOptions, _ = checker.GetProjectOptionsFromScript("test.fsx", input) |> Async.RunSynchronously
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projectOptions
        let parseResults = checker.ParseFile("test.fsx", input, parsingOptions) |> Async.RunSynchronously
        
        let rule =
            match rule with
            | LineRule rule -> rule
            | _ -> failwithf "TestLineRuleBase only accepts LineRules"
            
        input
        |> String.toLines
        |> Array.collect (fun (line, lineNumber, isLastLine) ->
            let lineRuleParams = {
                LineRuleParams.line = line
                lineNumber = lineNumber + 1
                isLastLine = isLastLine
                fileContent = input }
            rule.ruleConfig.runner lineRuleParams)
        |> Array.iter (suggestionToWarning "" >> this.postSuggestion)