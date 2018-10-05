namespace FSharpLint.Rules

module Types =

    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Configuration
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.ExpressionUtilities

    [<Literal>]
    let AnalyserName = "Types"
    
    let private isAnalyserEnabled config =
        isAnalyserEnabled config AnalyserName |> Option.isSome

    let private isRuleEnabled config ruleName = 
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    let private checkTypePrefixing args range typeName isPostfix isSuppressed =
        let ruleName = "TypePrefixing"

        let isEnabled = isRuleEnabled args.Info.Config ruleName

        if isEnabled && isSuppressed ruleName |> not then
            match typeName with
            | SynType.LongIdent lid ->
                match lid |> longIdentWithDotsToString with
                | "list"
                | "option"
                | "ref" as typeName ->
                    // Prefer postfix.
                    if not isPostfix
                    then 
                        let error = sprintf "Use postfix syntax for F# type %s" typeName
                        args.Info.Suggest { Range = range; Message = error; SuggestedFix = None; TypeChecks = [] }
                | "array" ->
                    // Prefer special postfix (e.g. int[]).
                    let error = "Use special postfix syntax for F# type array" 
                    args.Info.Suggest { Range = range; Message = error; SuggestedFix = None; TypeChecks = [] }
                | _ ->
                    // Prefer prefix.
                    if isPostfix
                    then 
                        let error = "Use prefix syntax for generic types" 
                        args.Info.Suggest { Range = range; Message = error; SuggestedFix = None; TypeChecks = [] }
            | _ ->
                // Prefer prefix.
                if isPostfix
                then 
                    let error = "Use prefix syntax for generic types" 
                    args.Info.Suggest { Range = range; Message = error; SuggestedFix = None; TypeChecks = [] }

    let analyser (args: AnalyserArgs) : unit = 

        if isAnalyserEnabled args.Info.Config then
            let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

            let isSuppressed i ruleName =
                AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
                |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
                
            for i = 0 to syntaxArray.Length - 1 do
                match syntaxArray.[i].Actual with
                | AstNode.Type (SynType.App (typeName, _, _, _, _, isPostfix, range)) ->
                    checkTypePrefixing args range typeName isPostfix (isSuppressed i)
                | _ -> ()