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

module SourceLength =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.TypeChecking
    open FSharpLint.Framework.Configuration

    let (|Member|Function|Value|Constructor|Property|) = function
        | SynValData.SynValData(memberFlags, valInfo, _) -> 
            match memberFlags with
                | Some(memberFlags) -> 
                    match memberFlags.MemberKind with
                        | MemberKind.Constructor(_)
                        | MemberKind.ClassConstructor(_) -> Constructor
                        | MemberKind.Member(_) -> Member
                        | MemberKind.PropertyGet(_)
                        | MemberKind.PropertySet(_)
                        | MemberKind.PropertyGetSet(_) -> Property
                | None when valInfo.ArgInfos.Length = 0 -> Value
                | None -> Function

    let configRuleSettings (config:Map<string,Analyser>) ruleName =
        if not <| config.ContainsKey "FSharpLint.SourceLength" then
            raise <| ConfigurationException("Expected FSharpLint.SourceLength analyser in config.")

        let rules = config.["FSharpLint.SourceLength"].Rules

        if not <| rules.ContainsKey ruleName then 
            let error = sprintf "Expected rule %s for FSharpLint.SourceLength analyser in config." ruleName
            raise <| ConfigurationException(error)

        let ruleSettings = rules.[ruleName].Settings

        let isEnabled = 
            if ruleSettings.ContainsKey "Enabled" then
                match ruleSettings.["Enabled"] with 
                    | Enabled(e) when true -> true
                    | _ -> false
            else
                false

        if isEnabled && ruleSettings.ContainsKey "Lines" then
            match ruleSettings.["Lines"] with
                | Lines(l) -> Some(l)
                | _ -> None
        else
            None

    let error name i actual = 
        sprintf "%ss should be less than %d lines long, was %d lines long." name i actual

    let inline length (range:range) = range.EndLine - range.StartLine

    let expectMaxLines visitorInfo range configRuleName errorName =
        let actualLines = length range

        match configRuleSettings visitorInfo.Config configRuleName with
            | Some(expectedMaxLines) when actualLines > expectedMaxLines ->
                visitorInfo.PostError range (error errorName expectedMaxLines actualLines)
            | _ -> ()
    
    let rec visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.Expression(SynExpr.Lambda(_, _, _, _, range)) -> 
                expectMaxLines visitorInfo range "MaxLinesInLambdaFunction" "Lambda Function"

                Continue
            | AstNode.Expression(SynExpr.MatchLambda(_, _, _, _, range)) -> 
                expectMaxLines visitorInfo range "MaxLinesInMatchLambdaFunction" "Match Lambda Function"

                Continue
            | AstNode.Binding(binding) ->
                match binding with
                    | SynBinding.Binding(_, _, _, _, attributes, _, valData, pattern, _, _, _, _) -> 
                        let length = length binding.RangeOfBindingAndRhs

                        let expectMaxLines = expectMaxLines visitorInfo binding.RangeOfBindingAndRhs

                        match valData with
                            | Value -> 
                                expectMaxLines "MaxLinesInValue" "Value" 
                            | Function -> 
                                expectMaxLines "MaxLinesInFunction" "Function" 
                            | Member -> 
                                expectMaxLines "MaxLinesInMember" "Member" 
                            | Constructor -> 
                                expectMaxLines "MaxLinesInConstructor" "Constructor" 
                            | Property -> 
                                expectMaxLines "MaxLinesInProperty" "Property"
                            | _ -> ()

                        Continue
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, isModule, _, _, _, _, range)) when isModule -> 
                expectMaxLines visitorInfo range "MaxLinesInModule" "Module"

                Continue
            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, repr, _, range)) ->
                match repr with
                    | SynTypeDefnRepr.Simple(simpleRepr, _) ->
                        match simpleRepr with
                            | SynTypeDefnSimpleRepr.Record(_) -> 
                                expectMaxLines visitorInfo range "MaxLinesInRecord" "Record"
                            | SynTypeDefnSimpleRepr.Enum(_) -> 
                                expectMaxLines visitorInfo range "MaxLinesInEnum" "Enum"
                            | SynTypeDefnSimpleRepr.Union(_) -> 
                                expectMaxLines visitorInfo range "MaxLinesInUnion" "Union"
                            | _ -> ()
                    | SynTypeDefnRepr.ObjectModel(_) -> 
                        expectMaxLines visitorInfo range "MaxLinesInClass" "Classes and interface"

                Continue
            | _ -> Continue