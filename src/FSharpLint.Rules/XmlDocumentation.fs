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

/// Rules to enforce the use of XML documentation in various places.
module XmlDocumentation =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "FSharpLint.XmlDocumentation"

    let checkExceptionHeader (config:Map<string,Analyser>) =
        if not <| config.ContainsKey AnalyserName then
            raise <| ConfigurationException(sprintf "Expected %s analyser in config." AnalyserName)

        let analyserSettings = config.[AnalyserName].Settings

        let isEnabled = 
            if analyserSettings.ContainsKey "Enabled" then
                match analyserSettings.["Enabled"] with 
                    | Enabled(e) when true -> true
                    | _ -> false
            else
                true

        let rules = config.[AnalyserName].Rules

        if isEnabled && rules.ContainsKey "ExceptionDefinitionHeader" then
            let ruleSettings = rules.["ExceptionDefinitionHeader"].Settings
            if ruleSettings.ContainsKey "Enabled" then
                match ruleSettings.["Enabled"] with
                    | Enabled(b) when b = true -> true
                    | _ -> false
            else
                false
        else
            false

    let isPreXmlDocEmpty (preXmlDoc:PreXmlDoc) =
        match preXmlDoc.ToXmlDoc() with
            | XmlDoc(lines) when Array.length lines = 0 -> true
            | _ -> false

    let visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, xmlDoc, _, range)) -> 
                if checkExceptionHeader visitorInfo.Config then
                    if isPreXmlDocEmpty xmlDoc then
                        visitorInfo.PostError range "Expected exception type to have xml documentation."
            | _ -> ()

        Continue