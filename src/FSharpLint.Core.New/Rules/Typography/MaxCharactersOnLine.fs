module FSharpLint.Rules.MaxCharactersOnLine

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Rules
open FSharp.Compiler.Range

[<RequireQualifiedAccess>]
type Config =
    { maxCharactersOnLine : int }

let checkMaxCharactersOnLine (config:Config) (args:LineRuleParams) =
    let maxCharacters = config.maxCharactersOnLine
    let lineLength = String.length args.line
    if lineLength > maxCharacters then
        let range = mkRange "" (mkPos args.lineNumber (maxCharacters + 1)) (mkPos args.lineNumber lineLength)
        let errorFormatString = Resources.GetString("RulesTypographyLineLengthError")
        { Range = range 
          Message = String.Format(errorFormatString, (maxCharacters + 1))
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton
    else
        Array.empty
        
let rule config =
    { name = "MaxCharactersOnLine"
      identifier = None
      ruleConfig = { LineRuleConfig.runner = checkMaxCharactersOnLine config } }
    |> LineRule