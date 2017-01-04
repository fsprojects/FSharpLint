// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.Rules

module Typography =

    open System
    open System.IO
    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Configuration
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open FSharpLint.Framework.Ast

    [<Literal>]
    let AnalyserName = "Typography"
    
    let private isAnalyserEnabled config =
        isAnalyserEnabled config AnalyserName |> Option.isSome

    let private isEnabled ruleName config =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    module private MaxCharactersOnLine =
        let maxCharactersOnLine config =
            match isRuleEnabled config AnalyserName "MaxCharactersOnLine" with
            | Some(_, ruleSettings) -> 
                match Map.tryFind "Length" ruleSettings with
                | Some(Length(length)) -> Some(length)
                | _ -> None
            | _ -> None

        let checkMaxCharactersOnLine mkRange analyserInfo line lineNumber isSuppressed maxCharacters =
            let lineLength = String.length line
            if lineLength > maxCharacters then
                let range = mkRange (mkPos lineNumber (maxCharacters + 1)) (mkPos lineNumber lineLength)
                if isSuppressed range "MaxCharactersOnLine" |> not then
                    let errorFormatString = Resources.GetString("RulesTypographyLineLengthError")
                    analyserInfo.Suggest
                        { Range = range 
                          Message = String.Format(errorFormatString, (maxCharacters + 1))
                          SuggestedFix = None }

    module private TrailingWhitespaceOnLine =
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

        let checkTrailingWhitespaceOnLine mkRange analyserInfo (line:string) lineNumber isSuppressed  =
            let ignoringBlankLinesAndIsBlankLine =
                isIgnoringBlankLines analyserInfo.Config && System.String.IsNullOrWhiteSpace(line)
                
            let stringEndsWithWhitespace =
                not ignoringBlankLinesAndIsBlankLine &&
                not <| doesStringNotEndWithWhitespace analyserInfo.Config line

            if stringEndsWithWhitespace then
                let whitespaceLength = lengthOfWhitespaceOnEnd line
                let range = mkRange (mkPos lineNumber (line.Length - whitespaceLength)) (mkPos lineNumber line.Length)
                if isSuppressed range "TrailingWhitespaceOnLine" |> not then
                    analyserInfo.Suggest
                        { Range = range 
                          Message = Resources.GetString("RulesTypographyTrailingWhitespaceError")
                          SuggestedFix = None }
                
    module private MaxLinesInFile =
        let private maxLinesInFile config =
            match isRuleEnabled config AnalyserName "MaxLinesInFile" with
            | Some(_, ruleSettings) -> 
                match Map.tryFind "Lines" ruleSettings with
                | Some(Lines(lines)) -> Some(lines)
                | Some(_) | None -> None
            | None -> None

        let private checkNumberOfLinesInFile mkRange analyserInfo numberOfLines line maxLines =
            if numberOfLines > maxLines then
                let errorFormatString = Resources.GetString("RulesTypographyFileLengthError")
                analyserInfo.Suggest 
                    { Range = mkRange (mkPos (maxLines + 1) 0) (mkPos numberOfLines (String.length line))
                      Message = String.Format(errorFormatString, (maxLines + 1))
                      SuggestedFix = None }

        let checkMaxLinesInFile mkRange analyserInfo numberOfLines lastLine =
            let checkNumberOfLinesInFile = checkNumberOfLinesInFile mkRange analyserInfo numberOfLines lastLine

            maxLinesInFile analyserInfo.Config |> Option.iter checkNumberOfLinesInFile

    module private TrailingNewLineInFile =
        let checkTrailingNewLineInFile mkRange analyserInfo numberOfLines =
            if isEnabled "TrailingNewLineInFile" analyserInfo.Config && analyserInfo.Text.EndsWith("\n") then
                let numberOfLinesIncludingTrailingNewLine = numberOfLines + 1
                let pos = mkPos numberOfLinesIncludingTrailingNewLine 0
                analyserInfo.Suggest 
                    { Range = mkRange pos pos
                      Message = Resources.GetString("RulesTypographyTrailingLineError")
                      SuggestedFix = None }

    module private NoTabCharacters =
        let checkNoTabCharacters mkRange analyserInfo (line:string) lineNumber isSuppressed isInLiteralString =
            let indexOfTab = line.IndexOf('\t')

            if indexOfTab >= 0 then
                let range = mkRange (mkPos lineNumber indexOfTab) (mkPos lineNumber (indexOfTab + 1))
                if (isSuppressed range "NoTabCharacters" || isInLiteralString range) |> not then
                    analyserInfo.Suggest 
                        { Range = range 
                          Message = Resources.GetString("RulesTypographyTabCharacterError")
                          SuggestedFix = None }

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
            
    let analyser (args: AnalyserArgs) : unit = 
        let syntaxArray = args.SyntaxArray

        if isAnalyserEnabled args.Info.Config then
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

            let maxCharacterOnLine = MaxCharactersOnLine.maxCharactersOnLine args.Info.Config 
            let noTabRuleEnabled = isEnabled "NoTabCharacters" args.Info.Config
            let trailingWhitespaceEnabled = isEnabled "TrailingWhitespaceOnLine" args.Info.Config

            let analyseLine line i isLastLineInFile =
                let lineNumber = i + 1

                maxCharacterOnLine
                |> Option.iter (MaxCharactersOnLine.checkMaxCharactersOnLine mkRange args.Info line lineNumber isSuppressed)
                
                if trailingWhitespaceEnabled then
                    TrailingWhitespaceOnLine.checkTrailingWhitespaceOnLine mkRange args.Info line lineNumber isSuppressed
                    
                if noTabRuleEnabled then
                    NoTabCharacters.checkNoTabCharacters mkRange args.Info line lineNumber isSuppressed isInLiteralString

                if isLastLineInFile then
                    TrailingNewLineInFile.checkTrailingNewLineInFile mkRange args.Info lineNumber
                    MaxLinesInFile.checkMaxLinesInFile mkRange args.Info lineNumber line

            args.Info.Text |> String.iterLine analyseLine