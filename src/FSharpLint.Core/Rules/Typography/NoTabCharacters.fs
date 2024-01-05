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
    literalStrings |> Seq.exists (fun (_, literalRange) -> ExpressionUtilities.rangeContainsOtherRange literalRange range)

let checkNoTabCharacters literalStrings (args:LineRuleParams) =
    let indexOfTab = args.Line.IndexOf('\t')

    if indexOfTab >= 0 then
        let range = Range.mkRange "" (Position.mkPos args.LineNumber indexOfTab) (Position.mkPos args.LineNumber (indexOfTab + 1))
        if isInLiteralString literalStrings range |> not then
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
              TypeChecks = [] } |> Array.singleton
        else
            Array.empty
    else
        Array.empty

let rule =
    { Name = "NoTabCharacters"
      Identifier = Identifiers.NoTabCharacters
      RuleConfig = { Runner = checkNoTabCharacters } }
    |> NoTabCharactersRule