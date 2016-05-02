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
    
    open System
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.AstInfo
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "SourceLength"
    (*
    let configLines config ruleName =
        match isRuleEnabled config AnalyserName ruleName with
        | Some(_, ruleSettings) when ruleSettings.ContainsKey "Lines" -> 
            match ruleSettings.["Lines"] with
            | Lines(l) -> Some(l)
            | _ -> None
        | Some(_)
        | None -> None

    let error name i actual = 
        let errorFormatString = Resources.GetString("RulesSourceLengthError")
        String.Format(errorFormatString, name, i, actual)

    let inline length (range:range) = range.EndLine - range.StartLine

    let expectMaxLines visitorInfo range configRuleName errorName =
        let actualLines = length range

        match configLines visitorInfo.Config configRuleName with
        | Some(expectedMaxLines) when actualLines > expectedMaxLines ->
            visitorInfo.PostError range (error errorName expectedMaxLines actualLines)
        | _ -> ()

    let visitor visitorInfo _ (syntaxArray:AbstractSyntaxArray.Node []) _ = 
        let mutable i = 0
        while i < syntaxArray.Length do
            match syntaxArray.[i].Actual with
            | AstNode.Expression(SynExpr.Lambda(_, _, _, _, range)) as node when (isLambdaALambdaArgument >> not) node -> 
                expectMaxLines visitorInfo range "MaxLinesInLambdaFunction" "Lambda function"
            | AstNode.Expression(SynExpr.MatchLambda(_, _, _, _, range)) -> 
                expectMaxLines visitorInfo range "MaxLinesInMatchLambdaFunction" "Match lambda function"
            | AstNode.Binding(SynBinding.Binding(_, _, _, _, _, _, valData, _, _, _, _, _) as binding) ->
                let expectMaxLines = expectMaxLines visitorInfo binding.RangeOfBindingAndRhs

                match identifierTypeFromValData valData with
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
                | Other -> ()
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_, isModule, _, _, _, _, range)) when isModule -> 
                expectMaxLines visitorInfo range "MaxLinesInModule" "Module"
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
            | _ -> ()

            i <- i + 1*)
    ()