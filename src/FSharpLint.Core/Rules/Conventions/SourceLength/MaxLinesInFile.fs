module FSharpLint.Rules.MaxLinesInFile

open System
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Rules
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
type Config = { MaxLinesInFile:int }

let private checkNumberOfLinesInFile numberOfLines line maxLines =
    if numberOfLines > maxLines then
        let violationTextFormatString = Resources.GetString "RulesTypographyFileLengthViolation"
        Array.singleton
            {
                Range =
                    Range.mkRange "" (Position.mkPos (maxLines + 1) 0) (Position.mkPos numberOfLines (String.length line))
                Message = String.Format(violationTextFormatString, (maxLines + 1))
                SuggestedFix = None
                TypeChecks = List.Empty
            }
    else
        Array.empty

let checkMaxLinesInFile (config:Config) (args:LineRuleParams) =
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
