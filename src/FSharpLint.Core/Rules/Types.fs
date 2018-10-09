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

    module private TypePrefixing =

        let checkTypePrefixing args range typeName typeArgs isPostfix isSuppressed =
            let ruleName = "TypePrefixing"

            let isEnabled = isRuleEnabled args.Info.Config ruleName

            if isEnabled && isSuppressed ruleName |> not then
                match typeName with
                | SynType.LongIdent lid ->
                    match lid |> longIdentWithDotsToString with
                    | "list"
                    | "List"
                    | "option"
                    | "Option"
                    | "ref"
                    | "Ref" as typeName ->
                        // Prefer postfix.
                        if not isPostfix
                        then 
                            let error = sprintf "Use postfix syntax for F# type %s" typeName
                            let suggestedFix = lazy(
                                (args.Info.TryFindTextOfRange range, typeArgs)
                                ||> Option.map2 (fun fromText typeArgs -> { FromText = fromText; FromRange = range; ToText = typeArgs + " " + typeName }))
                            args.Info.Suggest { Range = range; Message = error; SuggestedFix = Some suggestedFix; TypeChecks = [] }
                    | "array" ->
                        // Prefer special postfix (e.g. int[]).
                        let error = "Use special postfix syntax for F# type array" 
                        let suggestedFix = lazy(
                            (args.Info.TryFindTextOfRange range, typeArgs)
                            ||> Option.map2 (fun fromText typeArgs -> { FromText = fromText; FromRange = range; ToText = typeArgs + " []" }))
                        args.Info.Suggest { Range = range; Message = error; SuggestedFix = Some suggestedFix; TypeChecks = [] }
                    | typeName ->
                        // Prefer prefix.
                        if isPostfix
                        then 
                            let suggestedFix = lazy(
                                (args.Info.TryFindTextOfRange range, typeArgs)
                                ||> Option.map2 (fun fromText typeArgs -> { FromText = fromText; FromRange = range; ToText = typeName + "<" + typeArgs + ">" }))
                            let error = "Use prefix syntax for generic types" 
                            args.Info.Suggest { Range = range; Message = error; SuggestedFix = Some suggestedFix; TypeChecks = [] }
                | _ -> ()

    let analyser (args: AnalyserArgs) : unit = 

        let synTypeToString (synType:SynType) =
            args.Info.TryFindTextOfRange synType.Range

        let typeArgsToString (typeArgs:SynType list) =
            let typeStrings = typeArgs |> List.choose synTypeToString
            if typeStrings.Length = typeArgs.Length 
            then typeStrings |> String.concat "," |> Some
            else None

        if isAnalyserEnabled args.Info.Config then
            let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

            let isSuppressed i ruleName =
                AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
                |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
                
            for i = 0 to syntaxArray.Length - 1 do
                match syntaxArray.[i].Actual with
                | AstNode.Type (SynType.App (typeName, _, typeArgs, _, _, isPostfix, range)) ->
                    let typeArgs = typeArgsToString typeArgs
                    TypePrefixing.checkTypePrefixing args range typeName typeArgs isPostfix (isSuppressed i)
                | _ -> ()