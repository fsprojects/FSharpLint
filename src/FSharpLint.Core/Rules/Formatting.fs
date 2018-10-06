namespace FSharpLint.Rules

module Formatting =
    
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "Formatting"

    let private isRuleEnabled config ruleName = 
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    /// Checks for correct spacing around colon of typed expression.
    let private checkNamedTypeColonSpacing args range isSuppressed =
        let ruleName = "TypedItemSpacing"

        let isEnabled = isRuleEnabled args.Info.Config ruleName

        if isEnabled && isSuppressed ruleName |> not then
            args.Info.TryFindTextOfRange range
            |> Option.iter (fun text ->
                match text.Split(':') with
                | [|otherText; typeText|] ->
                    if otherText.TrimEnd(' ').Length <> otherText.Length - 1 
                    || typeText.TrimStart(' ').Length <> typeText.Length - 1 then
                        let suggestedFix = lazy(
                            { FromRange = range; FromText = text; ToText = otherText + " : " + typeText }
                            |> Some)
                        args.Info.Suggest 
                            { Range = range 
                              Message = "Use spaces around ':' in typed expression."
                              SuggestedFix = Some suggestedFix
                              TypeChecks = [] }
                | _ -> ())

    let analyser (args: AnalyserArgs) : unit = 
        let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

        let isSuppressed i ruleName =
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
            
        for i = 0 to syntaxArray.Length - 1 do
            match syntaxArray.[i].Actual with
            | AstNode.Pattern (SynPat.Typed (_, _, range)) ->
                checkNamedTypeColonSpacing args range (isSuppressed i) 
            | _ -> ()