namespace FSharpLint.Rules

module Conventions =

    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Configuration
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework.Ast

    [<Literal>]
    let AnalyserName = "Conventions"
    
    let private isAnalyserEnabled config =
        isAnalyserEnabled config AnalyserName |> Option.isSome

    let private isEnabled ruleName config =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    module private TopLevelNamespace =

        let checkTopLevelNamespace args range moduleOrNamespace isSuppressed =
            let ruleName = "TopLevelNamespace"

            let isEnabled = isEnabled ruleName args.Info.Config

            if isEnabled && isSuppressed ruleName |> not then
                let (SynModuleOrNamespace(_, _, isModule, _, _, _, _, _)) = moduleOrNamespace
                if isModule
                then
                    args.Info.Suggest
                        { Range = range 
                          Message = "Prefer namespaces at top level"
                          SuggestedFix = None
                          TypeChecks = [] }

    let analyser (args: AnalyserArgs) : unit = 
        if isAnalyserEnabled args.Info.Config then
            let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

            let isSuppressed i ruleName =
                AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
                |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
                
            syntaxArray
            |> Array.tryHead
            |> Option.iter (fun firstNode ->
                match firstNode.Actual with
                | AstNode.ModuleOrNamespace moduleOrNamespace ->
                    TopLevelNamespace.checkTopLevelNamespace args moduleOrNamespace.Range moduleOrNamespace (isSuppressed 0)
                | _ -> ())