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
    open FSharpLint.Framework.AstInfo
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors

    [<Literal>]
    let AnalyserName = "SourceLength"

    let configLines config ruleName =
        match isRuleEnabled config AnalyserName ruleName with
            | Some(_, ruleSettings) when ruleSettings.ContainsKey "Lines" -> 
                match ruleSettings.["Lines"] with
                    | Lines(l) -> Some(l)
                    | _ -> None
            | Some(_)
            | None -> None

    let error name i actual = 
        let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesSourceLengthError")
        System.String.Format(errorFormatString, name, i, actual)

    let inline length (range:range) = range.EndLine - range.StartLine

    let expectMaxLines visitorInfo (astNode:CurrentNode) range configRuleName errorName =
        let actualLines = length range

        match configLines visitorInfo.Config configRuleName with
            | Some(expectedMaxLines) when actualLines > expectedMaxLines && astNode.IsSuppressed(AnalyserName, configRuleName) |> not ->
                visitorInfo.PostError range (error errorName expectedMaxLines actualLines)
            | _ -> ()
    
    let rec visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.Expression(SynExpr.Lambda(_, _, _, _, range)) -> 
                expectMaxLines visitorInfo astNode range "MaxLinesInLambdaFunction" "Lambda function"

                Continue
            | AstNode.Expression(SynExpr.MatchLambda(_, _, _, _, range)) -> 
                expectMaxLines visitorInfo astNode range "MaxLinesInMatchLambdaFunction" "Match lambda function"

                Continue
            | AstNode.Binding(binding) ->
                match binding with
                    | SynBinding.Binding(_, _, _, _, attributes, _, valData, pattern, _, _, _, _) -> 
                        let length = length binding.RangeOfBindingAndRhs

                        let expectMaxLines = expectMaxLines visitorInfo astNode binding.RangeOfBindingAndRhs

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

                        Continue
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, isModule, _, _, _, _, range)) when isModule -> 
                expectMaxLines visitorInfo astNode range "MaxLinesInModule" "Module"

                Continue
            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, repr, _, range)) ->
                match repr with
                    | SynTypeDefnRepr.Simple(simpleRepr, _) ->
                        match simpleRepr with
                            | SynTypeDefnSimpleRepr.Record(_) -> 
                                expectMaxLines visitorInfo astNode range "MaxLinesInRecord" "Record"
                            | SynTypeDefnSimpleRepr.Enum(_) -> 
                                expectMaxLines visitorInfo astNode range "MaxLinesInEnum" "Enum"
                            | SynTypeDefnSimpleRepr.Union(_) -> 
                                expectMaxLines visitorInfo astNode range "MaxLinesInUnion" "Union"
                            | _ -> ()
                    | SynTypeDefnRepr.ObjectModel(_) -> 
                        expectMaxLines visitorInfo astNode range "MaxLinesInClass" "Classes and interface"

                Continue
            | _ -> Continue

    type RegisterSourceLengthVisitor() = 
        let plugin =
            {
                Name = AnalyserName
                Visitor = Ast(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin