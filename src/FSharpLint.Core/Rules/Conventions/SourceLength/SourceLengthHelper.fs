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
    }

type private MultilineCommentMarker =
    | Begin of charIndex: int
    | End of charIndex: int

let private singleLineCommentRegex = Regex(@"^[\s]*\/\/.*$", RegexOptions.Multiline)

let private multilineCommentMarkerRegex = Regex @"(\(\*[^\)])|([^\(]\*\))"
let private multilineCommentMarkerRegexCaptureGroupLength = 3

[<TailCall>]
let rec private getTopLevelBalancedPairs (toProcess: List<MultilineCommentMarker>) (stack: List<int>) : List<int*int> =
    match toProcess with
    | [] -> List.Empty
    | Begin(index)::tail ->
        getTopLevelBalancedPairs tail (index::stack)
    | End(index)::tail ->
        match stack with
        | [] -> List.Empty
        | [ beginIndex ] -> (beginIndex, index) :: getTopLevelBalancedPairs tail List.Empty
        | _::restOfStack -> getTopLevelBalancedPairs tail restOfStack

let checkSourceLengthRule (config:Config) range fileContents errorName (skipRanges: array<Range>) =
    let error name lineCount actual =
        let errorFormatString = Resources.GetString("RulesSourceLengthError")
        String.Format(errorFormatString, name, lineCount, actual)

    let stripMultilineComments (source: string) =
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

        getTopLevelBalancedPairs markers List.Empty
        |> List.fold
            (fun (currSource: string) (startIndex, endIndex) ->
                currSource.Substring(0, startIndex) 
                + currSource.Substring(endIndex + multilineCommentMarkerRegexCaptureGroupLength))
            source

    match tryFindTextOfRange range fileContents with
    | Some(sourceCode) -> 
        let sourceCode =
            stripMultilineComments sourceCode

        let commentLinesCount = 
            singleLineCommentRegex.Matches(sourceCode).Count

        let sourceCodeLines = sourceCode.Split([| '\n'; '\r' |]) 
        let blankLinesCount = 
            sourceCodeLines
            |> Seq.filter (fun line -> line.Trim().Length = 0)
            |> Seq.length

        let skippedLinesCount =
            skipRanges
            |> Seq.collect (fun skipRange -> seq { skipRange.StartLine .. skipRange.EndLine })
            |> Seq.distinct
            |> Seq.length

        let skipResult = sourceCodeLines.Length - commentLinesCount - blankLinesCount - skippedLinesCount
        if skipResult > config.MaxLines then
            Array.singleton
                {
                    Range = range
                    Message = error errorName config.MaxLines skipResult
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }
        else
            Array.empty
    | None -> Array.empty
