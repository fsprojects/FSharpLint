module FSharpLint.Rules.TrailingNewLineInFile

open System
open FSharpLint.Framework
open FSharpLint.Framework.Analyser
open FSharpLint.Framework.Rules
open FSharp.Compiler.Range

let checkTrailingNewLineInFile (args:LineRuleParams) =
    if args.isLastLine && args.fileContent.EndsWith("\n") then
        let numberOfLinesIncludingTrailingNewLine = args.lineNumber + 1
        let pos = mkPos numberOfLinesIncludingTrailingNewLine 0
        { Range = mkRange "" pos pos
          Message = Resources.GetString("RulesTypographyTrailingLineError")
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton
    else
        Array.empty

let rule =
    { name = "TrailingNewLineInFile"
      identifier = None
      ruleConfig = { LineRuleConfig.runner = checkTrailingNewLineInFile } }
    |> LineRule