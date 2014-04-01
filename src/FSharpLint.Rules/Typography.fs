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
    open FSharpLint.Framework.LoadAnalysers
    open Microsoft.FSharp.Compiler.Range

    [<Literal>]
    let AnalyserName = "FSharpLint.Typography"

    let isAnalyserEnabled config =
        (isAnalyserEnabled config AnalyserName).IsSome

    let isEnabled ruleName config =
        match isRuleEnabled config AnalyserName ruleName with
            | Some(_) -> true
            | None -> false

    let maxCharactersOnLine config =
        match isRuleEnabled config AnalyserName "MaxCharactersOnLine" with
            | Some(_, ruleSettings) when ruleSettings.ContainsKey "Length" -> 
                match ruleSettings.["Length"] with
                    | Length(length) -> Some(length)
                    | _ -> None
            | _ -> None

    let maxLinesInFile config =
        match isRuleEnabled config AnalyserName "MaxLinesInFile" with
            | Some(_, ruleSettings) when ruleSettings.ContainsKey "Lines" -> 
                match ruleSettings.["Lines"] with
                    | Lines(lines) -> Some(lines)
                    | _ -> None
            | _ -> None

    let endsWithWhitespace (str:string) =
        str.Length > 0 && System.Char.IsWhiteSpace(str.[str.Length - 1])

    let lengthOfWhitespaceOnEnd (str:string) =
        str.Length - str.TrimEnd().Length

    let analyseLine (visitorInfo:FSharpLint.Framework.Ast.VisitorInfo) mkRange lineNumber (line:string) = 
        let lineNumber = lineNumber + 1

        maxCharactersOnLine visitorInfo.Config
            |> Option.iter (fun maxCharacters ->
                if line.Length > maxCharacters then
                    let range = mkRange (mkPos lineNumber (maxCharacters + 1)) (mkPos lineNumber line.Length)
                    let error = sprintf "Lines should be less than %d characters long." (maxCharacters + 1)
                    visitorInfo.PostError range error)

        if isEnabled "TrailingWhitespaceOnLine" visitorInfo.Config && endsWithWhitespace line then
            let whitespaceLength = lengthOfWhitespaceOnEnd line
            let range = mkRange (mkPos lineNumber (line.Length - whitespaceLength)) (mkPos lineNumber line.Length)
            visitorInfo.PostError range "Lines should not have trailing whitespace."
                
        if isEnabled "NoTabCharacters" visitorInfo.Config then
            let indexOfTab = line.IndexOf('\t')

            if indexOfTab >= 0 then
                let range = mkRange (mkPos lineNumber indexOfTab) (mkPos lineNumber (indexOfTab + 1))
                visitorInfo.PostError range "File should not contain tab characters."
    
    let visitor (visitorInfo:FSharpLint.Framework.Ast.VisitorInfo) (file:string) filename = 
        if isAnalyserEnabled visitorInfo.Config then
            let mkRange = mkRange filename

            let lines = file.Split([|System.Environment.NewLine|], System.StringSplitOptions.None)

            if isEnabled "TrailingNewLineInFile" visitorInfo.Config && file.EndsWith("\n") then
                let range = mkRange (mkPos lines.Length 0) (mkPos lines.Length 0)
                visitorInfo.PostError range "File should not have a trailing new line."

            lines |> Array.iteri (analyseLine visitorInfo mkRange)

            maxLinesInFile visitorInfo.Config
                |> Option.iter (fun maxLines ->
                    if lines.Length > maxLines then
                        let range = mkRange (mkPos (maxLines + 1) 0) (mkPos lines.Length lines.[lines.Length - 1].Length)
                        let error = sprintf "Files should be less than %d lines long." (maxLines + 1)
                        visitorInfo.PostError range error)

    type RegisterXmlDocumentationAnalyser() = 
        let plugin =
            {
                Name = AnalyserName
                Analyser = PlainText(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin