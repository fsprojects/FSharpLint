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

        let checkTupleHasParentheses args parentNode range isSuppressed =
            let ruleName = "TupleParentheses"

            let isEnabled = isRuleEnabled args.Info.Config ruleName

            if isEnabled && isSuppressed ruleName |> not then
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

        let checkTupleCommaSpacing args range isSuppressed =
            let ruleName = "TupleCommaSpacing"

            let isEnabled = isRuleEnabled args.Info.Config ruleName

            if isEnabled && isSuppressed ruleName |> not then
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
                    | _ -> ())

    module private PatternMatchFormatting =

        let checkPatternMatchClausesOnNewLine args (clauses:SynMatchClause list) isSuppressed =
            let ruleName = "PatternMatchClausesOnNewLine"

            let isEnabled = isRuleEnabled args.Info.Config ruleName

            if isEnabled && isSuppressed ruleName |> not then
                clauses
                |> List.pairwise
                |> List.iter (fun (clauseOne, clauseTwo) -> 
                    if clauseOne.Range.EndLine = clauseTwo.Range.StartLine then
                        args.Info.Suggest
                            { Range = clauseTwo.Range
                              Message = "Each match clause should be placed on its own line"
                              SuggestedFix = None
                              TypeChecks = [] })

        let checkPatternMatchOrClausesOnNewLine args (clauses:SynMatchClause list) isSuppressed =
            let ruleName = "PatternMatchOrClausesOnNewLine"

            let isEnabled = isRuleEnabled args.Info.Config ruleName

            if isEnabled && isSuppressed ruleName |> not then
                clauses
                |> List.collect (function
                    | SynMatchClause.Clause (SynPat.Or (firstPat, secondPat, _), _, _, _, _) ->
                        [firstPat; secondPat]
                    | _ -> [])
                |> List.pairwise
                |> List.iter (fun (clauseOne, clauseTwo) -> 
                    if clauseOne.Range.EndLine = clauseTwo.Range.StartLine then
                        args.Info.Suggest
                            { Range = clauseTwo.Range
                              Message = "Each 'or' match clause should be placed on its own line"
                              SuggestedFix = None
                              TypeChecks = [] })

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
                TupleFormatting.checkTupleHasParentheses args parentNode tupleExpr.Range (isSuppressed i)
                TupleFormatting.checkTupleCommaSpacing args tupleExpr.Range (isSuppressed i)
            | AstNode.Expression (SynExpr.Match (_, _, clauses, _, _))
            | AstNode.Expression (SynExpr.MatchLambda (_, _, clauses, _, _)) ->
                PatternMatchFormatting.checkPatternMatchClausesOnNewLine args clauses (isSuppressed i)
                PatternMatchFormatting.checkPatternMatchOrClausesOnNewLine args clauses (isSuppressed i)
            | _ -> ()