module FSharpLint.Rules.Helper.SourceLength

open System
open System.Text.RegularExpressions
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Text
open FSharpLint.Framework.ExpressionUtilities

[<RequireQualifiedAccess>]
type Config = 
    { 
        MaxLines:int
        SkipBlankLines: bool
        SkipComments: bool
    }

let private error name i actual =
    let errorFormatString = Resources.GetString("RulesSourceLengthError")
    String.Format(errorFormatString, name, i, actual)

let private singleLineCommentRegex = Regex(@"^[\s]*\/\/.*$", RegexOptions.Multiline)

let checkSourceLengthRule (config:Config) range fileContents errorName =
    match tryFindTextOfRange range fileContents with
    | Some(sourceCode) -> 
        let commentLinesCount = 
            if config.SkipComments then
                singleLineCommentRegex.Matches(sourceCode).Count
            else
                0

        let sourceCodeLines = sourceCode.Split([| '\n'; '\r' |]) 
        let blankLinesCount = 
            if config.SkipBlankLines then
                sourceCodeLines
                |> Seq.filter (fun line -> line.Trim().Length > 0)
                |> Seq.length
            else
                0

        let skipResult = sourceCodeLines.Length - commentLinesCount - blankLinesCount
        if skipResult > config.MaxLines then
            { Range = range
              Message = error errorName config.MaxLines skipResult
              SuggestedFix = None
              TypeChecks = [] } |> Array.singleton
        else
            Array.empty
    | None -> Array.empty
