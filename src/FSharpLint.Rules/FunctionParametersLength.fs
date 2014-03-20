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

/// Checks if a function is declared with more than a configurable number of parameters.
module FunctionParametersLength =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    let maxParametersFromConfig (config:Map<string,Analyser>) =
        if not <| config.ContainsKey "FSharpLint.FunctionParametersLength" then
            raise <| ConfigurationException("Expected FSharpLint.FunctionParametersLength analyser in config.")

        let analyserSettings = config.["FSharpLint.FunctionParametersLength"].Settings

        let isEnabled = 
            if analyserSettings.ContainsKey "Enabled" then
                match analyserSettings.["Enabled"] with 
                    | Enabled(e) when true -> true
                    | _ -> false
            else
                false

        if isEnabled && analyserSettings.ContainsKey "MaxParameters" then
            match analyserSettings.["MaxParameters"] with
                | MaxParameters(p) -> Some(p)
                | _ -> None
        else
            None

    let error i = sprintf "Functions should have less than %d parameters" i
    
    let visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.Pattern(pattern) ->
                match pattern with
                    | SynPat.LongIdent(longIdentifier, identifier, _, constructorArguments, access, range) ->
                        match maxParametersFromConfig visitorInfo.Config with
                            | Some(maxParameters) ->
                                match constructorArguments with
                                    | SynConstructorArgs.Pats(patterns) when List.length patterns >= maxParameters -> 
                                        let failedPattern = patterns.[maxParameters - 1]
                                        visitorInfo.PostError failedPattern.Range (error maxParameters)
                                    | _ -> ()
                            | None -> ()
                    | _ -> ()
            | _ -> ()

        Continue