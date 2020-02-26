module FSharpLint.Rules.MaxLinesInFile

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Rules
open FSharp.Compiler.Range

[<RequireQualifiedAccess>]
type Config =
    {
        // fsharplint:disable RecordFieldNames
        maxLinesInFile : int
        // fsharplint:enable RecordFieldNames
    }

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
    if args.IsLastLine then
        checkNumberOfLinesInFile args.LineNumber args.Line config.maxLinesInFile
    else
        Array.empty

let rule config =
    { Name = "MaxLinesInFile"
      Identifier = Identifiers.MaxLinesInFile
      RuleConfig = { LineRuleConfig.Runner = checkMaxLinesInFile config } }
    |> LineRule