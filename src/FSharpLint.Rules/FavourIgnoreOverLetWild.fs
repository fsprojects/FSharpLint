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

/// Checks if any code uses 'let _ = ...' and suggests to use the ignore function.
module FavourIgnoreOverLetWild =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "FSharpLint.FavourIgnoreOverLetWild"

    let isEnabled (config:Map<string,Analyser>) =
        if not <| config.ContainsKey AnalyserName then
            raise <| ConfigurationException(sprintf "Expected %s analyser in config." AnalyserName)

        let analyserSettings = config.[AnalyserName].Settings

        if analyserSettings.ContainsKey "Enabled" then
            match analyserSettings.["Enabled"] with 
                | Enabled(e) when true -> true
                | _ -> false
        else
            false
    
    let visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.Binding(SynBinding.Binding(identifier, _, _, _, _, _, _, pattern, _, _, range, _)) -> 
                if isEnabled visitorInfo.Config then
                    let rec findWildAndIgnoreParens = function
                    | SynPat.Paren(pattern, _) -> findWildAndIgnoreParens pattern
                    | SynPat.Wild(_) -> true
                    | _ -> false
                
                    if findWildAndIgnoreParens pattern then
                        visitorInfo.PostError range "Favour using the ignore function rather than let _ = ..."
            | _ -> ()

        Continue