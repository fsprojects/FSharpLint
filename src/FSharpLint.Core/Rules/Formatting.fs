namespace FSharpLint.Rules

module Formatting =

    open System
    open FSharp.Compiler.Ast
    open FSharp.Compiler.Range
    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "Formatting"

    let private isRuleEnabled config ruleName =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    module private TypeDefinitionFormatting =

        let getUnionCaseStartColumn (args : AnalyserArgs) (SynUnionCase.UnionCase (attrs, _, _, _, _, range)) =
            match attrs |> List.tryHead with
            | Some attr ->
                mkRange "" (mkPos attr.Range.StartLine 0) attr.Range.Start
                |> args.Info.TryFindTextOfRange
                |> Option.bind (fun preceedingText ->
                    let attrStartIndex = preceedingText.IndexOf "[<"
                    if attrStartIndex = -1
                    then None
                    else Some attrStartIndex)
                |> Option.defaultValue (attr.Range.StartColumn - 2)
            | None ->
                range.StartColumn

        let checkUnionDefinitionIndentation args typeDefnRepr typeDefnStartColumn isSuppressed =
            let ruleName = "UnionDefinitionIndentation"

            let isEnabled = isRuleEnabled args.Info.Config ruleName

            if isEnabled && isSuppressed ruleName |> not then

                match typeDefnRepr with
                | SynTypeDefnRepr.Simple((SynTypeDefnSimpleRepr.Union (_, cases, _)), _) ->
                    match cases with
                    | []
                    | [_] -> ()
                    | firstCase :: _ ->
                        if getUnionCaseStartColumn args firstCase <> typeDefnStartColumn + 1 then
                          args.Info.Suggest
                            { Range = firstCase.Range
                              Message = Resources.GetString("RulesFormattingUnionDefinitionIndentationError")
                              SuggestedFix = None
                              TypeChecks = [] }

                        cases
                        |> List.pairwise
                        |> List.iter (fun (caseOne, caseTwo) ->
                            if getUnionCaseStartColumn args caseOne <> getUnionCaseStartColumn args caseTwo then
                                args.Info.Suggest
                                    { Range = caseTwo.Range
                                      Message = Resources.GetString("RulesFormattingUnionDefinitionSameIndentationError")
                                      SuggestedFix = None
                                      TypeChecks = [] })
                | _ -> ()

    let analyser (args: AnalyserArgs) : unit =
        let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

        let isSuppressed i ruleName =
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName

        for i = 0 to syntaxArray.Length - 1 do
            match syntaxArray.[i].Actual with
            | AstNode.TypeDefinition (SynTypeDefn.TypeDefn (_, repr, members, defnRange)) ->
                TypeDefinitionFormatting.checkUnionDefinitionIndentation args repr defnRange.StartColumn (isSuppressed i)
            | _ -> ()
