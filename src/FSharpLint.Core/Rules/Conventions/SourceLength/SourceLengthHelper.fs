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

type private MultilineCommentMarker =
    | Begin of int
    | End of int

let private error name i actual =
    let errorFormatString = Resources.GetString("RulesSourceLengthError")
    String.Format(errorFormatString, name, i, actual)

let private singleLineCommentRegex = Regex(@"^[\s]*\/\/.*$", RegexOptions.Multiline)

let private multilineCommentMarkerRegex = Regex @"(\(\*[^\)])|(\*\))"

let private stripMultilineComments (source: string) =
    let markers = 
        multilineCommentMarkerRegex.Matches source
        |> Seq.map (fun markerMatch -> 
            let index = markerMatch.Index
            if source.[index] = '(' then
                Begin index
            else
                End index)
        |> Seq.sortBy (function | Begin index -> index | End index -> index)
        |> Seq.toList

    let rec getTopLevelBalancedPairs (toProcess: List<MultilineCommentMarker>) (stack: List<int>) : List<int*int> =
        match toProcess with
        | [] -> []
        | Begin(index)::tail ->
            getTopLevelBalancedPairs tail (index::stack)
        | End(index)::tail ->
            match stack with
            | [] -> []
            | [ beginIndex ] -> (beginIndex, index) :: getTopLevelBalancedPairs tail []
            | _::restOfStack -> getTopLevelBalancedPairs tail restOfStack

    getTopLevelBalancedPairs markers []
    |> List.fold
        (fun (currSource: string) (startIndex, endIndex) ->
            currSource.Substring(0, startIndex) + currSource.Substring(endIndex + 2))
        source

let checkSourceLengthRule (config:Config) range fileContents errorName =
    match tryFindTextOfRange range fileContents with
    | Some(sourceCode) -> 
        let sourceCode =
            if config.SkipComments then
                stripMultilineComments sourceCode
            else
                sourceCode

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
