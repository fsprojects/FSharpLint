module FSharpLint.Rules.Helper.SourceLength

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Range

[<RequireQualifiedAccess>]
type Config =
    { maxLines : int }

let private error name i actual =
    let errorFormatString = Resources.GetString("RulesSourceLengthError")
    String.Format(errorFormatString, name, i, actual)

let private length (range:range) = range.EndLine - range.StartLine

let checkSourceLengthRule (config:Config) range errorName =
    let actualLines = length range
    if actualLines > config.maxLines then
        { Range = range
          Message = error errorName config.maxLines actualLines
          SuggestedFix = None
          TypeChecks = [] } |> Array.singleton

    else
        Array.empty
