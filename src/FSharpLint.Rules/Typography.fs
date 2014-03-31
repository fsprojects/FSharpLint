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

    let createRange () =
        mkRange "" (mkPos 1 1) (mkPos 1 1)
    
    let visitor (visitorInfo:FSharpLint.Framework.Ast.VisitorInfo) (file:string) = 
        if file.Contains("\t") then
            visitorInfo.PostError (createRange()) "File should not contain tab characters."

        if file.EndsWith("\n") then
            visitorInfo.PostError (createRange()) "File should not have a trailing new line."

        let lines = file.Split([|System.Environment.NewLine|], System.StringSplitOptions.None)

        lines |> Array.iter (fun x -> 
            if x.Length > 80 then
                visitorInfo.PostError (createRange()) "Lines should be less than 80 characters long."

            if lines |> Array.exists (fun x -> x.Length > 0 && System.Char.IsWhiteSpace(x.[x.Length - 1])) then
                visitorInfo.PostError (createRange()) "Lines should not have trailing whitespace.")

        if lines.Length > 500 then
            visitorInfo.PostError (createRange()) "Files should be less than 500 lines long."

    (*
    type RegisterXmlDocumentationAnalyser() = 
        let plugin =
            {
                Name = AnalyserName
                Analyser = PlainText(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin
    *)