module FSharpLint.Rules.TrailingNewLineInFile

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Rules
open FSharp.Compiler.Text

let checkTrailingNewLineInFile (args:LineRuleParams) =
    if args.IsLastLine && args.FileContent.EndsWith("\n") then
        let numberOfLinesIncludingTrailingNewLine = args.LineNumber + 1
        let pos = Pos.mkPos numberOfLinesIncludingTrailingNewLine 0
        { Range = Range.mkRange "" pos pos
          Message = Resources.GetString("RulesTypographyTrailingLineError")
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton
    else
        Array.empty

let rule =
    { Name = "TrailingNewLineInFile"
      Identifier = Identifiers.TrailingNewLineInFile
      RuleConfig = { LineRuleConfig.Runner = checkTrailingNewLineInFile } }
    |> LineRule