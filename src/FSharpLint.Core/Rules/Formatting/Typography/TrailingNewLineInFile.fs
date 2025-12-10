module FSharpLint.Rules.TrailingNewLineInFile

open System
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Rules
open FSharp.Compiler.Text

let checkTrailingNewLineInFile (args:LineRuleParams) =
    if args.IsLastLine && args.FileContent.EndsWith("\n") then
        let pos = Position.mkPos args.LineNumber 0
        Array.singleton
            {
                Range = Range.mkRange "" pos pos
                Message = Resources.GetString "RulesTypographyTrailingLineViolation"
                AutoFix = None
                TypeChecks = List.Empty
            }
    else
        Array.empty

let rule =
    LineRule
        {
            Name = "TrailingNewLineInFile"
            Identifier = Identifiers.TrailingNewLineInFile
            RuleConfig =
                {
                    LineRuleConfig.Runner = checkTrailingNewLineInFile
                }
        }
