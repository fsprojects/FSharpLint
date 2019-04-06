module TestAstNodeRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.Rules

[<AbstractClass>]
type TestAstNodeRuleBase (rule) =
    inherit TestRuleBase.TestRuleBase()
    
    override this.Parse (input:string) =
        let checker = FSharpChecker.Create()
        
        let projectOptions, _ = checker.GetProjectOptionsFromScript("test.fsx", input) |> Async.RunSynchronously
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projectOptions
        let parseResults = checker.ParseFile("test.fsx", input, parsingOptions) |> Async.RunSynchronously
        match parseResults.ParseTree with
        | Some tree ->
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray tree
            let suggestions =
                syntaxArray
                |> Array.mapi (fun n astNode -> (n, astNode))
                |> Array.collect (fun (i, astNode) ->
                    let getParents (depth:int) = AbstractSyntaxArray.getBreadcrumbs depth syntaxArray skipArray i
                    let astNodeParams =
                        { astNode = astNode.Actual
                          getParents = getParents
                          fileContent = input }
                    rule.runner astNodeParams)
            suggestions |> Array.iter (suggestionToWarning "" >> this.postSuggestion)
        | None ->
            ()