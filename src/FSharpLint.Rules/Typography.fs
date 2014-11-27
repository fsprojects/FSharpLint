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

    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors
    open Microsoft.FSharp.Compiler.Range
    open FSharpLint.Framework.Ast

    [<Literal>]
    let AnalyserName = "FSharpLint.Typography"

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

        let checkMaxCharactersOnLine mkRange (visitorInfo:FSharpLint.Framework.Ast.VisitorInfo) (line:string) lineNumber (plainTextVisitorInfo:PlainTextVisitorInfo) =
            let checkMaxCharactersOnLine maxCharacters =
                if line.Length > maxCharacters then
                    let range = mkRange (mkPos lineNumber (maxCharacters + 1)) (mkPos lineNumber line.Length)
                    if plainTextVisitorInfo.IsSuppressed(range, AnalyserName, "MaxCharactersOnLine") |> not then
                        let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesTypographyLineLengthError")
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
            let symbols = [
                '>';'<';'+';'-';'*';'=';'~';'%';'&';'|';'@'
                '#';'^';'!';'?';'/';'.';':';',';'(';')';'[';']';'{';'}'
            ]

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

        let checkTrailingWhitespaceOnLine mkRange (visitorInfo:VisitorInfo) (line:string) lineNumber (plainTextVisitorInfo:PlainTextVisitorInfo) =
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
                    if plainTextVisitorInfo.IsSuppressed(range, AnalyserName, "TrailingWhitespaceOnLine") |> not then
                        visitorInfo.PostError range (FSharpLint.Framework.Resources.GetString("RulesTypographyTrailingWhitespaceError"))
                
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
                let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesTypographyFileLengthError")
                let error = System.String.Format(errorFormatString, (maxLines + 1))
                visitorInfo.PostError range error

        let checkMaxLinesInFile mkRange (visitorInfo:FSharpLint.Framework.Ast.VisitorInfo) lines =
            let checkNumberOfLinesInFile = checkNumberOfLinesInFile mkRange visitorInfo lines

            maxLinesInFile visitorInfo.Config |> Option.iter checkNumberOfLinesInFile

    module TrailingNewLineInFile =
        let checkTrailingNewLineInFile mkRange (visitorInfo:VisitorInfo) (file:string) (lines:string []) =
            if isEnabled "TrailingNewLineInFile" visitorInfo.Config && file.EndsWith("\n") then
                let range = mkRange (mkPos lines.Length 0) (mkPos lines.Length 0)
                visitorInfo.PostError range (FSharpLint.Framework.Resources.GetString("RulesTypographyTrailingLineError"))

    module NoTabCharacters =
        let checkNoTabCharacters mkRange (visitorInfo:VisitorInfo) (line:string) lineNumber (plaintextVisitorInfo:PlainTextVisitorInfo) =
            if isEnabled "NoTabCharacters" visitorInfo.Config then
                let indexOfTab = line.IndexOf('\t')

                if indexOfTab >= 0 then
                    let range = mkRange (mkPos lineNumber indexOfTab) (mkPos lineNumber (indexOfTab + 1))
                    if plaintextVisitorInfo.IsSuppressed(range, AnalyserName, "NoTabCharacters") |> not then
                        visitorInfo.PostError range (FSharpLint.Framework.Resources.GetString("RulesTypographyTabCharacterError"))

    let analyseLine (visitorInfo:FSharpLint.Framework.Ast.VisitorInfo) mkRange suppressMessageAttributes lineNumber (line:string) = 
        let lineNumber = lineNumber + 1

        MaxCharactersOnLine.checkMaxCharactersOnLine mkRange visitorInfo line lineNumber suppressMessageAttributes
        TrailingWhitespaceOnLine.checkTrailingWhitespaceOnLine mkRange visitorInfo line lineNumber suppressMessageAttributes
        NoTabCharacters.checkNoTabCharacters mkRange visitorInfo line lineNumber suppressMessageAttributes

    let visitor (visitorInfo:VisitorInfo) (plaintextVisitorInfo:PlainTextVisitorInfo) = 
        if isAnalyserEnabled visitorInfo.Config then
            let mkRange = mkRange plaintextVisitorInfo.File

            let lines = 
                plaintextVisitorInfo.Input.Split([|"\n"|], System.StringSplitOptions.None)
                    |> Array.map (fun line -> line.TrimEnd('\r'))

            lines |> Array.iteri (analyseLine visitorInfo mkRange plaintextVisitorInfo)

            TrailingNewLineInFile.checkTrailingNewLineInFile mkRange visitorInfo plaintextVisitorInfo.Input lines
            MaxLinesInFile.checkMaxLinesInFile mkRange visitorInfo lines

    type RegisterTypographyVisitor() = 
        let plugin =
            {
                Name = AnalyserName
                Visitor = PlainText(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin