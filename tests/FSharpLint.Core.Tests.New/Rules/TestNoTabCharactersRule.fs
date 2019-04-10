module TestNoTabCharactersRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FSharpLint.Application.ConfigurationManager
open FSharpLint.Framework
open FSharpLint.Framework.Rules

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
            
        
        match parseResults.ParseTree with
        | Some tree -> 
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray tree
            let (_, context) = runAstNodeRules Array.empty input syntaxArray skipArray
            let lineRules = { LineRules.indentationRule = None; noTabCharactersRule = Some rule; genericLineRules = [||] }
         
            runLineRules lineRules input context
            |> Array.iter (suggestionToWarning "" >> this.postSuggestion)
        | None -> ()