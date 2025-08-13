module FSharpLint.Rules.NoTabCharacters

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

module ContextBuilder =

    let builder current astNode =
        match astNode with
        | Expression(SynExpr.Const(SynConst.String(value, _, _), range)) ->
            (value, range) :: current
        | _ ->
            current

let private isInLiteralString literalStrings range =
    Seq.exists (fun (_, literalRange) -> ExpressionUtilities.rangeContainsOtherRange literalRange range) literalStrings

let checkNoTabCharacters literalStrings (args:LineRuleParams) =
    let indexOfTab = args.Line.IndexOf('\t')

    if indexOfTab >= 0 then
        let range = Range.mkRange String.Empty (Position.mkPos args.LineNumber indexOfTab) (Position.mkPos args.LineNumber (indexOfTab + 1))
        if isInLiteralString literalStrings range |> not then
            Array.singleton
                { Range = range
                  Message = Resources.GetString("RulesTypographyTabCharacterError")
                  SuggestedFix =
                    Some(
                        lazy
                            (Some(
                                { FromRange = range
                                  FromText = "\t"
                                  ToText = String.replicate args.GlobalConfig.numIndentationSpaces " " }
                            ))
                    )
                  TypeChecks = List.Empty }
        else
            Array.empty
    else
        Array.empty

let rule =
    NoTabCharactersRule
        {
            Name = "NoTabCharacters"
            Identifier = Identifiers.NoTabCharacters
            RuleConfig = { Runner = checkNoTabCharacters }
        }
