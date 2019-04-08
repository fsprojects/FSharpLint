module TestNoTabCharactersRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<AbstractClass>]
type TestNoTabCharactersRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()
    
    override this.Parse (input:string) =
        let checker = FSharpChecker.Create()
        
        let projectOptions, _ = checker.GetProjectOptionsFromScript("test.fsx", input) |> Async.RunSynchronously
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projectOptions
        let parseResults = checker.ParseFile("test.fsx", input, parsingOptions) |> Async.RunSynchronously
        
        let rule =
            match rule with
            | NoTabCharactersRule rule -> rule
            | _ -> failwithf "TestNoTabCharactersRuleBase only accepts NoTabCharactersRules"
            
            
        let mutable state = List.empty
        match parseResults.ParseTree with
        | Some tree ->
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray tree
            syntaxArray |> Array.iter (fun astNode -> state <- NoTabCharacters.ContextBuilder.builder state astNode.Actual)
        | None ->
            ()
        
        input
        |> String.toLines
        |> Array.collect (fun (line, lineNumber, isLastLine) ->
            let lineRuleParams = {
                LineRuleParams.line = line
                lineNumber = lineNumber + 1
                isLastLine = isLastLine
                fileContent = input }
            rule.ruleConfig.runner state lineRuleParams)
        |> Array.iter (suggestionToWarning "" >> this.postSuggestion)