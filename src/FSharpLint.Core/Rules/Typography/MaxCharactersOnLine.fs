module FSharpLint.Rules.MaxCharactersOnLine

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Rules
open FSharp.Compiler.Range

[<RequireQualifiedAccess>]
type Config = { MaxCharactersOnLine : int }

let private checkMaxCharactersOnLine (config:Config) (args:LineRuleParams) =
    let maxCharacters = config.MaxCharactersOnLine
    let lineLength = String.length args.Line
    if lineLength > maxCharacters then
        let range = mkRange "" (mkPos args.LineNumber (maxCharacters + 1)) (mkPos args.LineNumber lineLength)
        { Range = range
          Message = Resources.Format("RulesTypographyLineLengthError", (maxCharacters + 1))
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton
    else
        Array.empty

let internal rule config =
    { Name = "MaxCharactersOnLine"
      Identifier = Identifiers.MaxCharactersOnLine
      RuleConfig = { LineRuleConfig.Runner = checkMaxCharactersOnLine config } }
    |> LineRule