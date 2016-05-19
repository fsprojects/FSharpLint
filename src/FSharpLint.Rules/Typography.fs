(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

namespace FSharpLint.Rules

module Typography =

    open FSharpLint.Framework
    open FSharpLint.Framework.Configuration
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open FSharpLint.Framework.Ast

    [<Literal>]
    let AnalyserName = "Typography"
    
    let isAnalyserEnabled config =
        isAnalyserEnabled config AnalyserName |> Option.isSome

    let isEnabled ruleName config =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    module MaxCharactersOnLine =
        let private maxCharactersOnLine config =
            match isRuleEnabled config AnalyserName "MaxCharactersOnLine" with
            | Some(_, ruleSettings) when ruleSettings.ContainsKey "Length" -> 
                match ruleSettings.["Length"] with
                | Length(length) -> Some(length)
                | _ -> None
            | _ -> None

        let checkMaxCharactersOnLine mkRange (visitorInfo:VisitorInfo) (line:string) lineNumber isSuppressed =
            let checkMaxCharactersOnLine maxCharacters =
                if line.Length > maxCharacters then
                    let range = mkRange (mkPos lineNumber (maxCharacters + 1)) (mkPos lineNumber line.Length)
                    if isSuppressed range "MaxCharactersOnLine" |> not then
                        let errorFormatString = Resources.GetString("RulesTypographyLineLengthError")
                        let error = System.String.Format(errorFormatString, (maxCharacters + 1))
                        visitorInfo.PostError range error
            
            maxCharactersOnLine visitorInfo.Config |> Option.iter checkMaxCharactersOnLine

    module TrailingWhitespaceOnLine =
        let private numberOfSpacesAllowed config =
            match isRuleEnabled config AnalyserName "TrailingWhitespaceOnLine" with
            | Some(_, ruleSettings) when ruleSettings.ContainsKey "NumberOfSpacesAllowed" -> 
                match ruleSettings.["NumberOfSpacesAllowed"] with
                | NumberOfSpacesAllowed(n) -> Some(n)
                | _ -> None
            | _ -> None

        let private isOneSpaceAllowedAfterOperator config =
            match isRuleEnabled config AnalyserName "TrailingWhitespaceOnLine" with
            | Some(_, ruleSettings) when ruleSettings.ContainsKey "OneSpaceAllowedAfterOperator" -> 
                match ruleSettings.["OneSpaceAllowedAfterOperator"] with
                | OneSpaceAllowedAfterOperator(b) -> Some(b)
                | _ -> None
            | _ -> None

        let private isIgnoringBlankLines config =
            match isRuleEnabled config AnalyserName "TrailingWhitespaceOnLine" with
            | Some(_, ruleSettings) when ruleSettings.ContainsKey "IgnoreBlankLines" -> 
                match ruleSettings.["IgnoreBlankLines"] with
                | IgnoreBlankLines(b) -> b
                | _ -> false
            | _ -> false

        let private isSymbol character = 
            let symbols = 
                [ '>';'<';'+';'-';'*';'=';'~';'%';'&';'|';'@'
                  '#';'^';'!';'?';'/';'.';':';',';'(';')';'[';']';'{';'}' ]

            symbols |> List.exists ((=) character) 

        let private doesStringNotEndWithWhitespace config (str:string) =
            match numberOfSpacesAllowed config, isOneSpaceAllowedAfterOperator config with
            | Some(numberOfSpacesAllowed), _ when numberOfSpacesAllowed > 0 ->
                str.Length - str.TrimEnd().Length <= numberOfSpacesAllowed
            | _, Some(isOneSpaceAllowedAfterOperator) when isOneSpaceAllowedAfterOperator ->
                let trimmedStr = str.TrimEnd()

                trimmedStr.Length = str.Length ||
                    (str.Length - trimmedStr.Length = 1 && 
                        trimmedStr.Length > 0 &&
                        isSymbol trimmedStr.[trimmedStr.Length - 1])
            | _ -> 
                str.TrimEnd().Length = str.Length

        let private lengthOfWhitespaceOnEnd (str:string) =
            str.Length - str.TrimEnd().Length

        let checkTrailingWhitespaceOnLine mkRange (visitorInfo:VisitorInfo) (line:string) lineNumber isSuppressed =
            let enabled = isEnabled "TrailingWhitespaceOnLine" visitorInfo.Config

            if enabled then
                let ignoringBlankLinesAndIsBlankLine =
                    isIgnoringBlankLines visitorInfo.Config && System.String.IsNullOrWhiteSpace(line)
                
                let stringEndsWithWhitespace =
                    not ignoringBlankLinesAndIsBlankLine &&
                    not <| doesStringNotEndWithWhitespace visitorInfo.Config line

                if stringEndsWithWhitespace then
                    let whitespaceLength = lengthOfWhitespaceOnEnd line
                    let range = mkRange (mkPos lineNumber (line.Length - whitespaceLength)) (mkPos lineNumber line.Length)
                    if isSuppressed range "TrailingWhitespaceOnLine" |> not then
                        visitorInfo.PostError range (Resources.GetString("RulesTypographyTrailingWhitespaceError"))
                
    module MaxLinesInFile =
        let private maxLinesInFile config =
            match isRuleEnabled config AnalyserName "MaxLinesInFile" with
            | Some(_, ruleSettings) when ruleSettings.ContainsKey "Lines" -> 
                match ruleSettings.["Lines"] with
                | Lines(lines) -> Some(lines)
                | _ -> None
            | _ -> None

        let private checkNumberOfLinesInFile mkRange (visitorInfo:VisitorInfo) (lines:string []) maxLines =
            if lines.Length > maxLines then
                let range = mkRange (mkPos (maxLines + 1) 0) (mkPos lines.Length lines.[lines.Length - 1].Length)
                let errorFormatString = Resources.GetString("RulesTypographyFileLengthError")
                let error = System.String.Format(errorFormatString, (maxLines + 1))
                visitorInfo.PostError range error

        let checkMaxLinesInFile mkRange (visitorInfo:VisitorInfo) lines =
            let checkNumberOfLinesInFile = checkNumberOfLinesInFile mkRange visitorInfo lines

            maxLinesInFile visitorInfo.Config |> Option.iter checkNumberOfLinesInFile

    module TrailingNewLineInFile =
        let checkTrailingNewLineInFile mkRange (visitorInfo:VisitorInfo) (file:string) (lines:string []) =
            if isEnabled "TrailingNewLineInFile" visitorInfo.Config && file.EndsWith("\n") then
                let range = mkRange (mkPos lines.Length 0) (mkPos lines.Length 0)
                visitorInfo.PostError range (Resources.GetString("RulesTypographyTrailingLineError"))

    module NoTabCharacters =
        let checkNoTabCharacters mkRange (visitorInfo:VisitorInfo) (line:string) lineNumber isSuppressed isInLiteralString =
            if isEnabled "NoTabCharacters" visitorInfo.Config then
                let indexOfTab = line.IndexOf('\t')

                if indexOfTab >= 0 then
                    let range = mkRange (mkPos lineNumber indexOfTab) (mkPos lineNumber (indexOfTab + 1))
                    let rangeContainsOtherRange (containingRange:range) (range:range) =
                        posGeq range.Start containingRange.Start &&
                        posGeq containingRange.End range.End
                    if (isSuppressed range "NoTabCharacters" || isInLiteralString range) |> not then
                        visitorInfo.PostError range (Resources.GetString("RulesTypographyTabCharacterError"))

    let analyseLine (visitorInfo:VisitorInfo) mkRange isSuppressed isInLiteralString lineNumber (line:string) = 
        let lineNumber = lineNumber + 1

        MaxCharactersOnLine.checkMaxCharactersOnLine mkRange visitorInfo line lineNumber isSuppressed
        TrailingWhitespaceOnLine.checkTrailingWhitespaceOnLine mkRange visitorInfo line lineNumber isSuppressed
        NoTabCharacters.checkNoTabCharacters mkRange visitorInfo line lineNumber isSuppressed isInLiteralString

    let visitor visitorInfo _ (syntaxArray:AbstractSyntaxArray.Node[]) _ = 
        if isAnalyserEnabled visitorInfo.Config then
            let mutable i = 0
            let suppressMessageAttributes = ResizeArray()
            let literalStrings = ResizeArray()
            while i < syntaxArray.Length do
                let node = syntaxArray.[i].Actual

                getSuppressMessageAttributes node
                |> List.iter suppressMessageAttributes.Add

                match node with
                | Expression(SynExpr.Const(SynConst.String(value, _), range)) -> 
                    literalStrings.Add(value, range)
                | _ -> ()

                i <- i + 1

            let rangeContainsOtherRange (containingRange:range) (range:range) =
                range.StartLine >= containingRange.StartLine && range.EndLine <= containingRange.EndLine

            let isInLiteralString range = 
                literalStrings |> Seq.exists (fun (_, literalRange) -> rangeContainsOtherRange literalRange range)

            let isSuppressed range rulename =
                let isAnalyserSuppressed (suppressedMessage:Ast.SuppressedMessage, suppressedMessageRange:range) =
                    suppressedMessage.Category = AnalyserName && 
                    (suppressedMessage.Rule = rulename || suppressedMessage.Rule = "*") &&
                    rangeContainsOtherRange suppressedMessageRange range

                suppressMessageAttributes |> Seq.exists isAnalyserSuppressed

            let mkRange = mkRange System.String.Empty

            let lines = 
                visitorInfo.Text.Split([|"\n"|], System.StringSplitOptions.None)
                |> Array.map (fun line -> line.TrimEnd('\r'))

            lines |> Array.iteri (analyseLine visitorInfo mkRange isSuppressed isInLiteralString)

            TrailingNewLineInFile.checkTrailingNewLineInFile mkRange visitorInfo visitorInfo.Text lines
            MaxLinesInFile.checkMaxLinesInFile mkRange visitorInfo lines