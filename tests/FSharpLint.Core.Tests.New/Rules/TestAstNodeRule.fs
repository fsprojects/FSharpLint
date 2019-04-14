module TestAstNodeRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Rules

[<AbstractClass>]
type TestAstNodeRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()
    
    override this.Parse (input:string) =
        let checker = FSharpChecker.Create()
        
        let projectOptions, _ = checker.GetProjectOptionsFromScript("test.fsx", input) |> Async.RunSynchronously
        let (parseResults, checkResults) = checker.ParseAndCheckFileInProject("test.fsx", 0, input, projectOptions) |> Async.RunSynchronously
        let rule =
            match rule with
            | AstNodeRule rule -> rule
            | _ -> failwithf "TestAstNodeRuleBase only accepts AstNodeRules"
            
        match parseResults.ParseTree with
        | Some tree ->
            let checkResult =
                match checkResults with
                | FSharpCheckFileAnswer.Succeeded result -> Some result
                | FSharpCheckFileAnswer.Aborted _ -> None
                
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray tree
            let suggestions = runAstNodeRules (Array.singleton rule) checkResult input syntaxArray skipArray |> fst
            rule.ruleConfig.cleanup()

            suggestions |> Array.iter this.postSuggestion
        | None ->
            ()