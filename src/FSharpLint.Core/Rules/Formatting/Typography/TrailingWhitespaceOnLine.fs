module FSharpLint.Rules.TrailingWhitespaceOnLine

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Rules
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
type Config =
    { NumberOfSpacesAllowed:int
      OneSpaceAllowedAfterOperator:bool
      IgnoreBlankLines:bool }

let checkTrailingWhitespaceOnLine (config:Config) (args:LineRuleParams) =
    let line = args.Line
    let lineNumber = args.LineNumber
    let ignoringBlankLinesAndIsBlankLine = config.IgnoreBlankLines && System.String.IsNullOrWhiteSpace(line)

    let isSymbol character =
        let symbols =
            [ '>';'<';'+';'-';'*';'=';'~';'%';'&';'|';'@'
              '#';'^';'!';'?';'/';'.';':';',';'(';')';'[';']';'{';'}' ]

        List.exists ((=) character) symbols

    let doesStringNotEndWithWhitespace (config:Config) (str:string) =
        match (config.NumberOfSpacesAllowed, config.OneSpaceAllowedAfterOperator) with
        | (numberOfSpacesAllowed, _) when numberOfSpacesAllowed > 0 ->
            str.Length - str.TrimEnd().Length <= numberOfSpacesAllowed
        | (_, isOneSpaceAllowedAfterOperator) when isOneSpaceAllowedAfterOperator ->
            let trimmedStr = str.TrimEnd()

            (trimmedStr.Length = str.Length)
            || (str.Length - trimmedStr.Length = 1
                && trimmedStr.Length > 0
                && isSymbol trimmedStr.[trimmedStr.Length - 1])
        | _ ->
            str.TrimEnd().Length = str.Length

    let stringEndsWithWhitespace =
        not ignoringBlankLinesAndIsBlankLine &&
        not <| doesStringNotEndWithWhitespace config line

    if stringEndsWithWhitespace then
        let whitespaceLength = line.Length - line.TrimEnd().Length
        let range = Range.mkRange String.Empty (Position.mkPos lineNumber (line.Length - whitespaceLength)) (Position.mkPos lineNumber line.Length)
        Array.singleton
            {
                Range = range
                Message = Resources.GetString("RulesTypographyTrailingWhitespaceError")
                SuggestedFix = None
                TypeChecks = List.Empty
            }
    else
        Array.empty

let rule config =
    LineRule
        {
            Name = "TrailingWhitespaceOnLine"
            Identifier = Identifiers.TrailingWhitespaceOnLine
            RuleConfig =
                {
                    LineRuleConfig.Runner = checkTrailingWhitespaceOnLine config
                }
        }
