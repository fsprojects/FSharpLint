module FSharpLint.Rules.Indentation

open System
open FSharpLint.Framework
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let [<Literal>] RuleName = "Indentation"

module ContextBuilder =

    let private firstRangePerLine (ranges:range list) =
        List.foldBack
            (fun (range:range) map -> Map.add range.StartLine range map)
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

    let private createAbsoluteAndOffsetOverrides expectedIndentation (rangeToUpdate:range) =
        let absoluteOverride = (rangeToUpdate.StartLine, (true, expectedIndentation))
        let relativeOverrides =
            [(rangeToUpdate.StartLine + 1)..rangeToUpdate.EndLine]
            |> List.map (fun offsetLine ->
                (offsetLine, (false, expectedIndentation)))
        (absoluteOverride::relativeOverrides)

    let private createAbsoluteAndOffsetOverridesBasedOnFirst (ranges:range list) =
        match ranges with
        | (first::others) ->
             let expectedIndentation = first.StartColumn
             others |> List.map (fun other -> (other.StartLine, (true, expectedIndentation)))
        | _ -> []

    let private indentationOverridesForNode (node:AstNode) =
        match node with
        | Expression(SynExpr.Record(recordFields=recordFields)) ->
            recordFields
            |> List.map (fun ((fieldName, _), _, _) -> fieldName.Range)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | Expression(SynExpr.ArrayOrListOfSeqExpr(expr=(SynExpr.CompExpr(isArrayOrList=true; expr=expr)))) ->
            extractSeqExprItems expr
            |> List.map (fun expr -> expr.Range)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | Expression(SynExpr.ArrayOrList(exprs=exprs)) ->
            exprs
            |> List.map (fun expr -> expr.Range)
            |> firstRangePerLine
            |> createAbsoluteAndOffsetOverridesBasedOnFirst
        | Expression(SynExpr.App(funcExpr=(SynExpr.App(funcExpr=SynExpr.Ident(ident); argExpr=innerArg)); argExpr=outerArg))
            when ident.idText = "op_PipeRight" ->
            let expectedIndentation = innerArg.Range.StartColumn
            createAbsoluteAndOffsetOverrides expectedIndentation outerArg.Range
        | Expression(SynExpr.ObjExpr(bindings=bindings; newExprRange=newExprRange)) ->
            let expectedIndentation = newExprRange.StartColumn + 4
            bindings
            |> List.map (fun binding -> binding.RangeOfBindingAndRhs)
            |> firstRangePerLine
            |> List.collect (createAbsoluteAndOffsetOverrides expectedIndentation)
        | _ -> []

    let builder current node =
        indentationOverridesForNode node
        |> List.fold (fun current (line, indentationOverride) ->
            Map.add line indentationOverride current) current

let checkIndentation (expectedSpaces:int) (line:string) (lineNumber:int) (indentationOverrides:Map<int,bool*int>) =
    let numLeadingSpaces = line.Length - line.TrimStart().Length
    let range = mkRange "" (mkPos lineNumber 0) (mkPos lineNumber numLeadingSpaces)

    if indentationOverrides.ContainsKey lineNumber then
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