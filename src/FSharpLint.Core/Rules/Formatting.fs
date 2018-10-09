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

    module private TypedItemSpacing =

        /// Checks for correct spacing around colon of typed expression.
        let checkTypedItemSpacing args range isSuppressed =
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

    module private TupleFormatting =

        let private checkTupleHasParentheses args parentNode range =
            match parentNode with
            | Some (AstNode.Expression (SynExpr.Paren _)) ->
                ()
            | _ ->
                args.Info.TryFindTextOfRange(range)
                |> Option.iter (fun text ->
                    let suggestedFix = lazy(
                        { FromRange = range; FromText = text; ToText = "(" + text + ")" }
                        |> Some)
                    args.Info.Suggest
                        { Range = range
                          Message = "Use parentheses for tuple instantiation."
                          SuggestedFix = Some suggestedFix
                          TypeChecks = [] })

        let private checkTupleCommaSpacing args range =
            args.Info.TryFindTextOfRange(range)
            |> Option.iter (fun text ->
                let splitText = text.Split(',') |> List.ofArray
                match splitText with
                | _ :: tail ->
                    if tail |> List.exists (fun item -> item.TrimStart().Length <> item.Length - 1) then
                        let fixedText =
                            splitText
                            |> List.map (fun (item:string) -> item.Trim())
                            |> String.concat ", "
                        let suggestedFix = lazy(
                            { FromRange = range 
                              FromText = text
                              ToText = fixedText } 
                            |> Some)
                        args.Info.Suggest
                            { Range = range
                              Message = "Comma in tuple instantiation should be followed by single space."
                              SuggestedFix = Some suggestedFix
                              TypeChecks = [] }
                | _ -> ()
            )

        let checkTupleFormatting args (tupleExpr:SynExpr) parentNode isSuppressed =
            let ruleName = "TupleFormatting"

            let isEnabled = isRuleEnabled args.Info.Config ruleName

            if isEnabled && isSuppressed ruleName |> not then
                match tupleExpr with
                | SynExpr.Tuple (_, _, range) ->
                    checkTupleCommaSpacing args range
                    checkTupleHasParentheses args parentNode range
                | _ -> ()

    let analyser (args: AnalyserArgs) : unit = 
        let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

        let isSuppressed i ruleName =
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
            
        for i = 0 to syntaxArray.Length - 1 do
            match syntaxArray.[i].Actual with
            | AstNode.Pattern (SynPat.Typed (_, _, range)) ->
                TypedItemSpacing.checkTypedItemSpacing args range (isSuppressed i) 
            | AstNode.Expression (SynExpr.Tuple _ as tupleExpr) ->
                let parentNode = AbstractSyntaxArray.getBreadcrumbs 1 syntaxArray skipArray i |> List.tryHead
                TupleFormatting.checkTupleFormatting args tupleExpr parentNode (isSuppressed i)
            | _ -> ()