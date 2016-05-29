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

    open System.IO
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
        let maxCharactersOnLine config =
            match isRuleEnabled config AnalyserName "MaxCharactersOnLine" with
            | Some(_, ruleSettings) -> 
                match Map.tryFind "Length" ruleSettings with
                | Some(Length(length)) -> Some(length)
                | _ -> None
            | _ -> None

        let checkMaxCharactersOnLine mkRange (visitorInfo:VisitorInfo) line lineNumber isSuppressed maxCharacters =
            let lineLength = String.length line
            if lineLength > maxCharacters then
                let range = mkRange (mkPos lineNumber (maxCharacters + 1)) (mkPos lineNumber lineLength)
                if isSuppressed range "MaxCharactersOnLine" |> not then
                    let errorFormatString = Resources.GetString("RulesTypographyLineLengthError")
                    let error = System.String.Format(errorFormatString, (maxCharacters + 1))
                    visitorInfo.PostError range error

    module TrailingWhitespaceOnLine =
        let numberOfSpacesAllowed config =
            match isRuleEnabled config AnalyserName "TrailingWhitespaceOnLine" with
            | Some(_, ruleSettings) -> 
                match Map.tryFind "NumberOfSpacesAllowed" ruleSettings with
                | Some(NumberOfSpacesAllowed(n)) -> Some(n)
                | _ -> None
            | _ -> None

        let isOneSpaceAllowedAfterOperator config =
            match isRuleEnabled config AnalyserName "TrailingWhitespaceOnLine" with
            | Some(_, ruleSettings) -> 
                match Map.tryFind "OneSpaceAllowedAfterOperator" ruleSettings with
                | Some(OneSpaceAllowedAfterOperator(b)) -> Some(b)
                | _ -> None
            | _ -> None

        let isIgnoringBlankLines config =
            match isRuleEnabled config AnalyserName "TrailingWhitespaceOnLine" with
            | Some(_, ruleSettings) -> 
                match Map.tryFind "IgnoreBlankLines" ruleSettings with
                | Some(IgnoreBlankLines(b)) -> b
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

        let checkTrailingWhitespaceOnLine mkRange (visitorInfo:VisitorInfo) (line:string) lineNumber isSuppressed  =
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

        let private checkNumberOfLinesInFile mkRange (visitorInfo:VisitorInfo) numberOfLines line maxLines =
            if numberOfLines > maxLines then
                let range = mkRange (mkPos (maxLines + 1) 0) (mkPos numberOfLines (String.length line))
                let errorFormatString = Resources.GetString("RulesTypographyFileLengthError")
                let error = System.String.Format(errorFormatString, (maxLines + 1))
                visitorInfo.PostError range error

        let checkMaxLinesInFile mkRange (visitorInfo:VisitorInfo) numberOfLines lastLine =
            let checkNumberOfLinesInFile = checkNumberOfLinesInFile mkRange visitorInfo numberOfLines lastLine

            maxLinesInFile visitorInfo.Config |> Option.iter checkNumberOfLinesInFile

    module TrailingNewLineInFile =
        let checkTrailingNewLineInFile mkRange (visitorInfo:VisitorInfo) (file:string) numberOfLines =
            if isEnabled "TrailingNewLineInFile" visitorInfo.Config && file.EndsWith("\n") then
                let range = mkRange (mkPos numberOfLines 0) (mkPos numberOfLines 0)
                visitorInfo.PostError range (Resources.GetString("RulesTypographyTrailingLineError"))

    module NoTabCharacters =
        let checkNoTabCharacters mkRange (visitorInfo:VisitorInfo) (line:string) lineNumber isSuppressed isInLiteralString =
            let indexOfTab = line.IndexOf('\t')

            if indexOfTab >= 0 then
                let range = mkRange (mkPos lineNumber indexOfTab) (mkPos lineNumber (indexOfTab + 1))
                if (isSuppressed range "NoTabCharacters" || isInLiteralString range) |> not then
                    visitorInfo.PostError range (Resources.GetString("RulesTypographyTabCharacterError"))

    module private String =
        let iterLine f input =
            use reader = new StringReader(input)

            let readLine () = 
                match reader.ReadLine() with
                | null -> None
                | line -> Some line

            let rec iterateLines currentLine i = 
                match currentLine with
                | Some line ->
                    let nextLine = readLine ()
                    let isLastLine = Option.isNone nextLine

                    f line i isLastLine

                    iterateLines nextLine (i + 1)
                | None -> ()

            iterateLines (readLine ()) 0

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

            let maxCharacterOnLine = MaxCharactersOnLine.maxCharactersOnLine visitorInfo.Config 
            let noTabRuleEnabled = isEnabled "NoTabCharacters" visitorInfo.Config
            let trailingWhitespaceEnabled = isEnabled "TrailingWhitespaceOnLine" visitorInfo.Config

            visitorInfo.Text
            |> String.iterLine (fun line i atEnd -> 
                let lineNumber = i + 1

                maxCharacterOnLine
                |> Option.iter (MaxCharactersOnLine.checkMaxCharactersOnLine mkRange visitorInfo line lineNumber isSuppressed)
                
                if trailingWhitespaceEnabled then
                    TrailingWhitespaceOnLine.checkTrailingWhitespaceOnLine mkRange visitorInfo line lineNumber isSuppressed
                    
                if noTabRuleEnabled then
                    NoTabCharacters.checkNoTabCharacters mkRange visitorInfo line lineNumber isSuppressed isInLiteralString

                if atEnd then
                    let totalLines = lineNumber
                    TrailingNewLineInFile.checkTrailingNewLineInFile mkRange visitorInfo visitorInfo.Text totalLines
                    MaxLinesInFile.checkMaxLinesInFile mkRange visitorInfo totalLines line)