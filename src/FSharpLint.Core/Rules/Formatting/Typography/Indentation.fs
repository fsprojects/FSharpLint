module FSharpLint.Rules.Indentation

open System
open FSharpLint.Framework
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let [<Literal>] RuleName = "Indentation"

module ContextBuilder =

    let private firstRangePerLine (ranges:Range list) =
        List.foldBack
            (fun (range:Range) map -> Map.add range.StartLine range map)
            ranges
            Map.empty
        |> Map.toList
        |> List.unzip
        |> snd

    let private extractSeqExprItems seqExpr =
        let rec helper items = function
            | SynExpr.Sequential(expr1=expr1; expr2=expr2) ->
                helper (expr1::items) expr2
            | other ->
                (other::items)

        helper List.Empty seqExpr
        |> List.rev

    let private createAbsoluteAndOffsetOverrides expectedIndentation (rangeToUpdate:Range) =
        let absoluteOverride = (rangeToUpdate.StartLine, (true, expectedIndentation))
        let relativeOverrides =
            List.map (fun offsetLine ->
                (offsetLine, (false, expectedIndentation))) [(rangeToUpdate.StartLine + 1)..rangeToUpdate.EndLine]
        (absoluteOverride::relativeOverrides)

    let rec private collectRecordFields = function
        | (SynExpr.Record ( _, _, fields, _)) ->
            let subRecords =
                fields
                |> List.choose (fun (SynExprRecordField(_, _, expr, _)) -> Option.map collectRecordFields expr)
                |> List.concat
            fields::subRecords
        | _ ->
            List.Empty

    let private createAbsoluteAndOffsetOverridesBasedOnFirst (ranges:Range list) =
        match ranges with
        | (first::others) ->
            let expectedIndentation = first.StartColumn
            List.collect
                (fun (other: Range) ->
                    [
                        for lineNumber = other.StartLine to other.EndLine do
                            yield (lineNumber, (true, expectedIndentation))
                    ])
                others
        | _ -> List.Empty

    let private indentationOverridesForNode (node:AstNode) =
        match node with
        | TypeDefinition (SynTypeDefn(_, SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _), _, _, _, _)) ->
            fields
            |> List.map (fun (SynField (_, _, _, _, _, _, _, range, _)) -> range)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | Expression (SynExpr.Tuple (_, exprs, _, _)) ->
            exprs
            |> List.map (fun expr -> expr.Range)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | Expression (SynExpr.Record _ as record) ->
            collectRecordFields record
            |> List.collect (fun recordFields ->
                recordFields
                |> List.map (fun (SynExprRecordField((fieldName, _), _, _, _)) -> fieldName.Range)
                |> firstRangePerLine
                |> createAbsoluteAndOffsetOverridesBasedOnFirst)
        | Expression (SynExpr.ArrayOrListComputed(expr=(SynExpr.ComputationExpr(expr=expr)))) ->
            extractSeqExprItems expr
            |> List.map (fun expr -> expr.Range)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | Expression (SynExpr.ArrayOrListComputed(expr=expr)) ->
            extractSeqExprItems expr
            |> List.map (fun expr -> expr.Range)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | Expression (SynExpr.ArrayOrList(exprs=exprs)) ->
            exprs
            |> List.map (fun expr -> expr.Range)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | Expression (SynExpr.App(funcExpr=(SynExpr.App(isInfix=isInfix; argExpr=innerArg; funcExpr=funcExpr)); argExpr=outerArg))
            when isInfix && outerArg.Range.EndLine <> innerArg.Range.StartLine ->
            match funcExpr with
            | ExpressionUtilities.Identifier([ ident ], _) when ident.idText = "op_ColonEquals" ->
                // := for reference cell assignment should be handled like normal equals, not like an infix operator.
                List.Empty
            | _ ->
                let expectedIndentation = innerArg.Range.StartColumn
                createAbsoluteAndOffsetOverrides expectedIndentation outerArg.Range
        | Expression (SynExpr.ObjExpr(members=members; bindings=bindings; newExprRange=newExprRange)) ->
            let expectedIndentation = newExprRange.StartColumn + 4
            let bindingOverrides =
                bindings
                |> List.map (fun binding -> binding.RangeOfBindingWithRhs)
                |> firstRangePerLine
                |> List.collect (createAbsoluteAndOffsetOverrides expectedIndentation)
            let memberOverrides =
                members
                |> List.map (fun mem -> mem.Range)
                |> firstRangePerLine
                |> List.collect (createAbsoluteAndOffsetOverrides expectedIndentation)
            List.append bindingOverrides memberOverrides
        | Pattern (SynPat.Tuple (elementPats=elemPats)) ->
            elemPats
            |> List.map (fun pat -> pat.Range)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | Pattern (SynPat.Record (fieldPats=fieldPats)) ->
            fieldPats
            |> List.map (fun ((_, fieldIdent), _, _) -> fieldIdent.idRange)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | _ -> List.Empty

    let builder current node =
        indentationOverridesForNode node
        |> List.fold (fun current (line, indentationOverride) ->
            Map.add line indentationOverride current) current

let checkIndentation (expectedSpaces:int) (line:string) (lineNumber:int) (indentationOverrides:Map<int,bool*int>) =
    let lineTrimmedStart = line.TrimStart()
    let numLeadingSpaces = line.Length - lineTrimmedStart.Length
    let range = Range.mkRange String.Empty (Position.mkPos lineNumber 0) (Position.mkPos lineNumber numLeadingSpaces)

    if lineTrimmedStart.StartsWith "//" || lineTrimmedStart.StartsWith "(*" then
        None
    elif indentationOverrides.ContainsKey lineNumber then
        match indentationOverrides.[lineNumber] with
        | (true, expectedIndentation) ->
            if numLeadingSpaces <> expectedIndentation then
                let violationMsg = Resources.GetString "RulesTypographyOverridenIndentationViolation"
                Some
                    {
                        Range = range
                        Message = violationMsg
                        AutoFix = None
                        TypeChecks = List.Empty
                    }
            else
                None
        | (false, indentationOffset) ->
            if (numLeadingSpaces - indentationOffset) % expectedSpaces <> 0 then
                let violationTextFormatString = Resources.GetString "RulesTypographyOverridenIndentationViolation"

                Some
                    {
                        Range = range
                        Message = String.Format(violationTextFormatString, expectedSpaces)
                        AutoFix = None
                        TypeChecks = List.Empty
                    }
            else
                None
    elif numLeadingSpaces % expectedSpaces <> 0 then
        let violationTextFormatString = Resources.GetString "RulesTypographyIndentationViolation"
        Some
            {
                Range = range
                Message = String.Format(violationTextFormatString, expectedSpaces)
                AutoFix = None
                TypeChecks = List.Empty
            }
    else
        None

let runner context args =
    checkIndentation args.GlobalConfig.numIndentationSpaces args.Line args.LineNumber context
    |> Option.toArray

let rule =
    IndentationRule
        {
            Name = "Indentation"
            Identifier = Identifiers.Indentation
            RuleConfig = { Runner = runner }
        }
