module FSharpLint.Rules.MaxCharactersOnLine

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Rules
open FSharp.Compiler.Range

[<RequireQualifiedAccess>]
type Config =
    {
        // fsharplint:disable RecordFieldNames
        maxCharactersOnLine : int
        // fsharplint:enable RecordFieldNames
    }

let checkMaxCharactersOnLine (config:Config) (args:LineRuleParams) =
    let maxCharacters = config.maxCharactersOnLine
    let lineLength = String.length args.Line
    if lineLength > maxCharacters then
        let range = mkRange "" (mkPos args.LineNumber (maxCharacters + 1)) (mkPos args.LineNumber lineLength)
        let errorFormatString = Resources.GetString("RulesTypographyLineLengthError")
        { Range = range
          Message = String.Format(errorFormatString, (maxCharacters + 1))
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton
    else
        Array.empty

let rule config =
    { Name = "MaxCharactersOnLine"
      Identifier = Identifiers.MaxCharactersOnLine
      RuleConfig = { LineRuleConfig.Runner = checkMaxCharactersOnLine config } }
    |> LineRule