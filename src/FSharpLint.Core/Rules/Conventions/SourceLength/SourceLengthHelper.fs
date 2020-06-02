module FSharpLint.Rules.Helper.SourceLength

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Range

[<RequireQualifiedAccess>]
type Config = { MaxLines : int }

let private length (range:range) = range.EndLine - range.StartLine

let internal checkSourceLengthRule (config:Config) range errorName =
    let actualLines = length range
    if actualLines > config.MaxLines then
        { Range = range
          Message = Resources.Format("RulesSourceLengthError", errorName, config.MaxLines, actualLines)
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton

    else
        Array.empty
