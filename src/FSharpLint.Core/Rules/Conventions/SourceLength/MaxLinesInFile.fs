module FSharpLint.Rules.MaxLinesInFile

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Rules
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
type Config = { MaxLinesInFile:int }

let checkMaxLinesInFile (config:Config) (args:LineRuleParams) =
    let checkNumberOfLinesInFile numberOfLines line maxLines =
        if numberOfLines > maxLines then
            let errorFormatString = Resources.GetString("RulesTypographyFileLengthError")
            Array.singleton
                {
                    Range =
                        Range.mkRange String.Empty (Position.mkPos (maxLines + 1) 0) (Position.mkPos numberOfLines (String.length line))
                    Message = String.Format(errorFormatString, (maxLines + 1))
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }
        else
            Array.empty

    if args.IsLastLine then
        checkNumberOfLinesInFile args.LineNumber args.Line config.MaxLinesInFile
    else
        Array.empty

let rule config =
    LineRule
        {
            Name = "MaxLinesInFile"
            Identifier = Identifiers.MaxLinesInFile
            RuleConfig =
                {
                    LineRuleConfig.Runner = checkMaxLinesInFile config
                }
        }
