module TestIndentationRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FSharpLint.Application.ConfigurationManager
open FSharpLint.Framework
open FSharpLint.Framework.Rules

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
            
        
        match parseResults.ParseTree with
        | Some tree -> 
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray tree
            let (_, context) = runAstNodeRules Array.empty input syntaxArray skipArray
            let lineRules = { LineRules.indentationRule = Some rule; noTabCharactersRule = None; genericLineRules = [||] }
         
            runLineRules lineRules input context
            |> Array.iter (suggestionToWarning "" >> this.postSuggestion)
        | None -> ()