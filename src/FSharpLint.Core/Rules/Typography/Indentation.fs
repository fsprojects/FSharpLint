module FSharpLint.Rules.Indentation

open System
open FSharpLint.Framework
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Suggestion
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

        helper [] seqExpr
        |> List.rev

    let private createAbsoluteAndOffsetOverrides expectedIndentation (rangeToUpdate:Range) =
        let absoluteOverride = (rangeToUpdate.StartLine, (true, expectedIndentation))
        let relativeOverrides =
            [(rangeToUpdate.StartLine + 1)..rangeToUpdate.EndLine]
            |> List.map (fun offsetLine ->
                (offsetLine, (false, expectedIndentation)))
        (absoluteOverride::relativeOverrides)

    let rec private collectRecordFields = function
        | (SynExpr.Record ( _, _, fields, _)) ->
            let subRecords =
                fields
                |> List.choose (fun (_, expr, _) -> expr |> Option.map collectRecordFields)
                |> List.concat
            fields::subRecords
        | _ ->
            []

    let private createAbsoluteAndOffsetOverridesBasedOnFirst (ranges:Range list) =
        match ranges with
        | (first::others) ->
            let expectedIndentation = first.StartColumn
            others |> List.map (fun other -> (other.StartLine, (true, expectedIndentation)))
        | _ -> []

    let private indentationOverridesForNode (node:AstNode) =
        match node with
        | TypeDefinition (SynTypeDefn(_, SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _), _,_,  _)) ->
            fields
            |> List.map (fun (SynField (_, _, _, _, _, _, _, range)) -> range)
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
                |> List.map (fun ((fieldName, _), _, _) -> fieldName.Range)
                |> firstRangePerLine
                |> createAbsoluteAndOffsetOverridesBasedOnFirst)
        | Expression (SynExpr.ArrayOrListOfSeqExpr(expr=(SynExpr.CompExpr(isArrayOrList=true; expr=expr)))) ->
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
            | SynExpr.Ident ident when ident.idText = "op_ColonEquals" ->
                // := for reference cell assignment should be handled like normal equals, not like an infix operator.
                []
            | _ ->
                let expectedIndentation = innerArg.Range.StartColumn
                createAbsoluteAndOffsetOverrides expectedIndentation outerArg.Range
        | Expression (SynExpr.ObjExpr(bindings=bindings; newExprRange=newExprRange)) ->
            let expectedIndentation = newExprRange.StartColumn + 4
            bindings
            |> List.map (fun binding -> binding.RangeOfBindingWithRhs)
            |> firstRangePerLine
            |> List.collect (createAbsoluteAndOffsetOverrides expectedIndentation)
        | Pattern (SynPat.Tuple (elementPats=elemPats)) ->
            elemPats
            |> List.map (fun pat -> pat.Range)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | Pattern (SynPat.Record (fieldPats=fieldPats)) ->
            fieldPats
            |> List.map (fun ((_, fieldIdent), _) -> fieldIdent.idRange)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | _ -> []

    let builder current node =
        indentationOverridesForNode node
        |> List.fold (fun current (line, indentationOverride) ->
            Map.add line indentationOverride current) current

let checkIndentation (expectedSpaces:int) (line:string) (lineNumber:int) (indentationOverrides:Map<int,bool*int>) =
    let lineTrimmedStart = line.TrimStart()
    let numLeadingSpaces = line.Length - lineTrimmedStart.Length
    let range = Range.mkRange "" (Position.mkPos lineNumber 0) (Position.mkPos lineNumber numLeadingSpaces)

    if lineTrimmedStart.StartsWith "//" || lineTrimmedStart.StartsWith "(*" then
        None
    elif indentationOverrides.ContainsKey lineNumber then
        match indentationOverrides.[lineNumber] with
        | (true, expectedIndentation) ->
            if numLeadingSpaces <> expectedIndentation then
                let errorString = Resources.GetString("RulesTypographyOverridenIndentationError")
                { Range = range
                  Message =  errorString
                  SuggestedFix = None
                  TypeChecks = [] } |> Some
            else
                None
        | (false, indentationOffset) ->
            if (numLeadingSpaces - indentationOffset) % expectedSpaces <> 0 then
                let errorFormatString = Resources.GetString("RulesTypographyOverridenIndentationError")
                { Range = range
                  Message =  String.Format(errorFormatString, expectedSpaces)
                  SuggestedFix = None
                  TypeChecks = [] } |> Some
            else
                None
    elif numLeadingSpaces % expectedSpaces <> 0 then
        let errorFormatString = Resources.GetString("RulesTypographyIndentationError")
        { Range = range
          Message =  String.Format(errorFormatString, expectedSpaces)
          SuggestedFix = None
          TypeChecks = [] } |> Some
    else
        None

let runner context args =
    checkIndentation args.GlobalConfig.numIndentationSpaces args.Line args.LineNumber context
    |> Option.toArray

let rule =
    { Name = "Indentation"
      Identifier = Identifiers.Indentation
      RuleConfig = { Runner = runner } }
    |> IndentationRule