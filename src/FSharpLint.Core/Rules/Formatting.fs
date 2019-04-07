namespace FSharpLint.Rules

module Formatting =

    open System
    open FSharp.Compiler.Ast
    open FSharp.Compiler.Range
    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.ExpressionUtilities

    [<Literal>]
    let AnalyserName = "Formatting"

    let private isRuleEnabled config ruleName =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    module private Spacing =

        let countPrecedingCommentLines (args : AnalyserArgs) (startPos : pos) (endPos : pos) =
            mkRange
                ""
                startPos
                endPos
            |> args.Info.TryFindTextOfRange
            |> Option.map (fun preceedingText ->
                let lines =
                    preceedingText.Split '\n'
                    |> Array.rev
                    |> Array.tail
                lines
                |> Array.takeWhile (fun line -> line.TrimStart().StartsWith("//"))
                |> Array.length)
            |> Option.defaultValue 0

        let checkModuleDeclSpacing args synModuleOrNamespace isSuppressed =
            let ruleName = "ModuleDeclSpacing"

            let isEnabled = isRuleEnabled args.Info.Config ruleName

            if isEnabled && isSuppressed ruleName |> not then

                match synModuleOrNamespace with
                | SynModuleOrNamespace (_, _, _, decls, _, _, _, _) ->
                    decls
                    |> List.pairwise
                    |> List.iter (fun (declOne, declTwo) ->
                        let numPreceedingCommentLines = countPrecedingCommentLines args declOne.Range.End declTwo.Range.Start
                        if declTwo.Range.StartLine <> declOne.Range.EndLine + 3 + numPreceedingCommentLines then
                            let intermediateRange =
                                let startLine = declOne.Range.EndLine + 1
                                let endLine = declTwo.Range.StartLine
                                let endOffset =
                                    if startLine = endLine
                                    then 1
                                    else 0

                                mkRange
                                    ""
                                    (mkPos (declOne.Range.EndLine + 1) 0)
                                    (mkPos (declTwo.Range.StartLine + endOffset) 0)
                            args.Info.Suggest
                                { Range = intermediateRange
                                  Message = Resources.GetString("RulesFormattingModuleDeclSpacingError")
                                  SuggestedFix = None
                                  TypeChecks = [] })

        let checkClassMemberSpacing args (members:SynMemberDefns) isSuppressed =
            let ruleName = "ClassMemberSpacing"

            let isEnabled = isRuleEnabled args.Info.Config ruleName

            if isEnabled && isSuppressed ruleName |> not then
                members
                |> List.pairwise
                |> List.iter (fun (memberOne, memberTwo) ->
                    let numPreceedingCommentLines = countPrecedingCommentLines args memberOne.Range.End memberTwo.Range.Start
                    if memberTwo.Range.StartLine <> memberOne.Range.EndLine + 2 + numPreceedingCommentLines then
                        let intermediateRange =
                            let startLine = memberOne.Range.EndLine + 1
                            let endLine = memberTwo.Range.StartLine
                            let endOffset =
                                if startLine = endLine
                                then 1
                                else 0

                            mkRange
                                ""
                                (mkPos (memberOne.Range.EndLine + 1) 0)
                                (mkPos (memberTwo.Range.StartLine + endOffset) 0)
                        args.Info.Suggest
                            { Range = intermediateRange
                              Message = Resources.GetString("RulesFormattingClassMemberSpacingError")
                              SuggestedFix = None
                              TypeChecks = [] })

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
            | AstNode.ModuleOrNamespace synModuleOrNamespace ->
                Spacing.checkModuleDeclSpacing args synModuleOrNamespace (isSuppressed i)
            | AstNode.TypeDefinition (SynTypeDefn.TypeDefn (_, repr, members, defnRange)) ->
                Spacing.checkClassMemberSpacing args members (isSuppressed i)
                TypeDefinitionFormatting.checkUnionDefinitionIndentation args repr defnRange.StartColumn (isSuppressed i)
            | AstNode.TypeRepresentation (SynTypeDefnRepr.ObjectModel (_, members, _)) ->
                Spacing.checkClassMemberSpacing args members (isSuppressed i)
            | _ -> ()
