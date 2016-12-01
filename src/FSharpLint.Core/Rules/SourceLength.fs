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

module SourceLength =
    
    open System
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.AstInfo
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "SourceLength"

    let private error name i actual = 
        let errorFormatString = Resources.GetString("RulesSourceLengthError")
        String.Format(errorFormatString, name, i, actual)

    let private length (range:range) = range.EndLine - range.StartLine

    let analyser visitorInfo _ syntaxArray skipArray = 
        let checkRuleBroken i range ruleName errorName =
            match isRuleEnabled visitorInfo.Config AnalyserName ruleName with
            | Some(_, ruleSettings) -> 
                let actualLines = length range
                match Map.tryFind "Lines" ruleSettings with
                | Some(Lines(maxExpectedLines)) when actualLines > maxExpectedLines -> 
                    let isSuppressed =
                        AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
                        |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
                
                    if not isSuppressed then 
                        visitorInfo.Suggest 
                            { Range = range 
                              Message = error errorName maxExpectedLines actualLines
                              SuggestedFix = None }
                | Some(_) | None -> ()
            | None -> ()

        let mutable i = 0
        while i < syntaxArray.Length do
            match syntaxArray.[i].Actual with
            | AstNode.Expression(SynExpr.Lambda(_, _, _, _, range)) -> 
                checkRuleBroken i range "MaxLinesInLambdaFunction" "Lambda function"
            | AstNode.Expression(SynExpr.MatchLambda(_, _, _, _, range)) -> 
                checkRuleBroken i range "MaxLinesInMatchLambdaFunction" "Match lambda function"
            | AstNode.Binding(SynBinding.Binding(_, _, _, _, _, _, valData, _, _, _, _, _) as binding) ->
                match identifierTypeFromValData valData with
                | Value -> 
                    checkRuleBroken i binding.RangeOfBindingAndRhs "MaxLinesInValue" "Value"
                | Function -> 
                    checkRuleBroken i binding.RangeOfBindingAndRhs "MaxLinesInFunction" "Function" 
                | Member -> 
                    checkRuleBroken i binding.RangeOfBindingAndRhs "MaxLinesInMember" "Member" 
                | Constructor -> 
                    checkRuleBroken i binding.RangeOfBindingAndRhs "MaxLinesInConstructor" "Constructor" 
                | Property -> 
                    checkRuleBroken i binding.RangeOfBindingAndRhs "MaxLinesInProperty" "Property"
                | Other -> ()
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_, _, isModule, _, _, _, _, range)) when isModule -> 
                checkRuleBroken i range "MaxLinesInModule" "Module"
            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, repr, _, range)) ->
                match repr with
                | SynTypeDefnRepr.Simple(simpleRepr, _) ->
                    match simpleRepr with
                    | SynTypeDefnSimpleRepr.Record(_) -> 
                        checkRuleBroken i range "MaxLinesInRecord" "Record"
                    | SynTypeDefnSimpleRepr.Enum(_) -> 
                        checkRuleBroken i range "MaxLinesInEnum" "Enum"
                    | SynTypeDefnSimpleRepr.Union(_) -> 
                        checkRuleBroken i range "MaxLinesInUnion" "Union"
                    | _ -> ()
                | SynTypeDefnRepr.ObjectModel(_) -> 
                    checkRuleBroken i range "MaxLinesInClass" "Classes and interface"
                | SynTypeDefnRepr.Exception(_) -> ()
            | _ -> ()

            i <- i + 1