module TestAstNodeRuleBase

open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework
open FSharpLint.Framework.ParseFile
open FSharpLint.Framework.Rules

[<AbstractClass>]
type TestAstNodeRuleBase (rule:Rule) =
    inherit TestRuleBase.TestRuleBase()
    
    override this.Parse (input:string, ?fileName:string) =
        let checker = FSharpChecker.Create()
        
        let parseResults =
            match fileName with
            | Some fileName ->
                ParseFile.parseSourceFile fileName input () checker
            | None ->
                ParseFile.parseSource input () checker

        let rule =
            match rule with
            | AstNodeRule rule -> rule
            | _ -> failwithf "TestAstNodeRuleBase only accepts AstNodeRules"
            
        match parseResults with
        | ParseFileResult.Success parseInfo ->
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray parseInfo.Ast
            let suggestions = runAstNodeRules (Array.singleton rule) parseInfo.TypeCheckResults input syntaxArray skipArray |> fst
            rule.ruleConfig.cleanup()

            suggestions |> Array.iter this.postSuggestion
        | _ ->
            failwithf "Failed to parse"