module TestIndentationRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities

[<AbstractClass>]
type TestIndentationRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()
    
    override this.Parse (input:string) =
        let checker = FSharpChecker.Create()
        
        let projectOptions, _ = checker.GetProjectOptionsFromScript("test.fsx", input) |> Async.RunSynchronously
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projectOptions
        let parseResults = checker.ParseFile("test.fsx", input, parsingOptions) |> Async.RunSynchronously
        
        let rule =
            match rule with
            | IndentationRule rule -> rule
            | _ -> failwithf "TestIndentationRuleBase only accepts IndentationRules"
            
            
        let mutable state = Map.empty
        rule.ruleConfig.astFolder
        |> Option.iter (fun builder ->
            match parseResults.ParseTree with
            | Some tree ->
                let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray tree
                syntaxArray
                |> Array.iter (fun astNode -> state <- builder state astNode.Actual)
            | None ->
                ())
        
        input
        |> String.toLines
        |> Array.collect (fun (line, lineNumber, isLastLine) ->
            let lineRuleParams = {
                LineRuleParams.line = line
                lineNumber = lineNumber + 1
                fileContent = input }
            rule.ruleConfig.runner state lineRuleParams)
        |> Array.iter (suggestionToWarning "" >> this.postSuggestion)