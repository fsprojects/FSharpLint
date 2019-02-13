namespace FSharpLint.Rules

module SourceLength =

    open System
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.AstInfo
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "SourceLength"

    let private error name i actual =
        let errorFormatString = Resources.GetString("RulesSourceLengthError")
        String.Format(errorFormatString, name, i, actual)

    let private length (range:range) = range.EndLine - range.StartLine

    let analyser (args: AnalyserArgs) : unit =
        let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

        let checkRuleBroken i range ruleName errorName =
            match isRuleEnabled args.Info.Config AnalyserName ruleName with
            | Some(_, ruleSettings) ->
                let actualLines = length range
                match Map.tryFind "Lines" ruleSettings with
                | Some(Lines(maxExpectedLines)) when actualLines > maxExpectedLines ->
                    let isSuppressed =
                        AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i
                        |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName

                    if not isSuppressed then
                        args.Info.Suggest
                            { Range = range
                              Message = error errorName maxExpectedLines actualLines
                              SuggestedFix = None
                              TypeChecks = [] }
                | Some(_) | None -> ()
            | None -> ()

        for i = 0 to syntaxArray.Length - 1 do
            match syntaxArray.[i].Actual with
            | AstNode.Expression(SynExpr.Lambda(_, _, _, _, range)) ->
                checkRuleBroken i range "MaxLinesInLambdaFunction" "Lambda function"
            | AstNode.Expression(SynExpr.MatchLambda(_, _, _, _, range)) ->
                checkRuleBroken i range "MaxLinesInMatchLambdaFunction" "Match lambda function"
            | AstNode.Binding(SynBinding.Binding(_, _, _, _, _, _, valData, _, _, _, _, _) as binding) ->
                match identifierTypeFromValData valData with
                | Value ->
                    checkRuleBroken i binding.RangeOfBindingAndRhs "MaxLinesInValue" "Value"
                | Function ->
                    checkRuleBroken i binding.RangeOfBindingAndRhs "MaxLinesInFunction" "Function"
                | Member ->
                    checkRuleBroken i binding.RangeOfBindingAndRhs "MaxLinesInMember" "Member"
                | Constructor ->
                    checkRuleBroken i binding.RangeOfBindingAndRhs "MaxLinesInConstructor" "Constructor"
                | Property ->
                    checkRuleBroken i binding.RangeOfBindingAndRhs "MaxLinesInProperty" "Property"
                | Other -> ()
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_, _, (NamedModule | AnonModule), _, _, _, _, range)) ->
                checkRuleBroken i range "MaxLinesInModule" "Module"
            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, repr, _, range)) ->
                match repr with
                | SynTypeDefnRepr.Simple(simpleRepr, _) ->
                    match simpleRepr with
                    | SynTypeDefnSimpleRepr.Record(_) ->
                        checkRuleBroken i range "MaxLinesInRecord" "Record"
                    | SynTypeDefnSimpleRepr.Enum(_) ->
                        checkRuleBroken i range "MaxLinesInEnum" "Enum"
                    | SynTypeDefnSimpleRepr.Union(_) ->
                        checkRuleBroken i range "MaxLinesInUnion" "Union"
                    | _ -> ()
                | SynTypeDefnRepr.ObjectModel(_) ->
                    checkRuleBroken i range "MaxLinesInClass" "Classes and interface"
                | SynTypeDefnRepr.Exception(_) -> ()
            | _ -> ()
