module FSharpLint.Rules.MaxLinesInFile

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Rules
open FSharp.Compiler.Range

[<RequireQualifiedAccess>]
type Config =
    { maxLinesInFile : int }

let private checkNumberOfLinesInFile numberOfLines line maxLines =
    if numberOfLines > maxLines then
        let errorFormatString = Resources.GetString("RulesTypographyFileLengthError")
        { Range = mkRange "" (mkPos (maxLines + 1) 0) (mkPos numberOfLines (String.length line))
          Message = String.Format(errorFormatString, (maxLines + 1))
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton
    else
        Array.empty

let checkMaxLinesInFile (config:Config) (args:LineRuleParams) =
    if args.isLastLine then
        checkNumberOfLinesInFile args.lineNumber args.line config.maxLinesInFile
    else
        Array.empty

let rule config =
    { name = "MaxLinesInFile"
      identifier = None
      ruleConfig = { LineRuleConfig.runner = checkMaxLinesInFile config } }
    |> LineRule