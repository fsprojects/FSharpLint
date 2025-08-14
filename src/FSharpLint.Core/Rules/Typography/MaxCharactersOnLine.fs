module FSharpLint.Rules.MaxCharactersOnLine

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Rules
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
type Config = { MaxCharactersOnLine:int }

let checkMaxCharactersOnLine (config:Config) (args:LineRuleParams) =
    let maxCharacters = config.MaxCharactersOnLine
    let lineLength = String.length args.Line
    if lineLength > maxCharacters then
        let range = Range.mkRange String.Empty (Position.mkPos args.LineNumber (maxCharacters + 1)) (Position.mkPos args.LineNumber lineLength)
        let errorFormatString = Resources.GetString("RulesTypographyLineLengthError")
        Array.singleton
            {
                Range = range
                Message = String.Format(errorFormatString, (maxCharacters + 1))
                Fix = None
                TypeChecks = List.Empty
            }
    else
        Array.empty

let rule config =
    LineRule
        {
            Name = "MaxCharactersOnLine"
            Identifier = Identifiers.MaxCharactersOnLine
            RuleConfig =
                {
                    LineRuleConfig.Runner = checkMaxCharactersOnLine config
                }
        }
