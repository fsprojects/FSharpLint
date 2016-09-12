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
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.AstInfo

    type SourceLengthRule(analyserName, name, errorName, code, config) =
        inherit Rule(analyserName, name, code, config)

        let formatString = Resources.GetString "SourceLength"

        let length (range:range) = range.EndLine - range.StartLine

        let maxLines =
            match Configuration.isRuleEnabled config analyserName name with
            | Some(_, ruleSettings) -> 
                match Map.tryFind "Lines" ruleSettings with
                | Some(Configuration.Lines(maxExpectedLines)) -> Some(maxExpectedLines)
                | Some(_) | None -> None
            | None -> None

        member this.Check(analysisArgs, i, range) = 
            match maxLines with
            | Some(maxExpectedLines) ->
                let actualLines = length range
                if actualLines > maxExpectedLines && this.NotSuppressed analysisArgs i then
                    let message = this.MessageFormat(maxExpectedLines, actualLines)
                    analysisArgs.Context.PostError range message
            | None -> ()

        member __.MessageFormat(maxExpectedLines, actualLines) = 
            String.Format(formatString, errorName, maxExpectedLines, actualLines)

    [<Sealed>]
    type SourceLengthAnalyser(config) =
        inherit Analyser.Analyser(name = "SourceLength", code = "3", config = config)

        member this.MaxLinesInLambdaFunction =
            SourceLengthRule(this.Name, "MaxLinesInLambdaFunction", "Lambda function", "1", config)

        member this.MaxLinesInMatchLambdaFunction =
            SourceLengthRule(this.Name, "MaxLinesInMatchLambdaFunction", "Match lambda function", "2", config)

        member this.MaxLinesInValue =
            SourceLengthRule(this.Name, "MaxLinesInValue", "Value", "3", config)

        member this.MaxLinesInFunction =
            SourceLengthRule(this.Name, "MaxLinesInFunction", "Function", "4", config)

        member this.MaxLinesInMember =
            SourceLengthRule(this.Name, "MaxLinesInMember", "Member", "5", config)

        member this.MaxLinesInConstructor =
            SourceLengthRule(this.Name, "MaxLinesInConstructor", "Constructor", "6", config)

        member this.MaxLinesInProperty =
            SourceLengthRule(this.Name, "MaxLinesInProperty", "Property", "7", config)

        member this.MaxLinesInModule =
            SourceLengthRule(this.Name, "MaxLinesInModule", "Module", "8", config)

        member this.MaxLinesInRecord =
            SourceLengthRule(this.Name, "MaxLinesInRecord", "Record", "9", config)

        member this.MaxLinesInEnum =
            SourceLengthRule(this.Name, "MaxLinesInEnum", "Enum", "10", config)

        member this.MaxLinesInUnion =
            SourceLengthRule(this.Name, "MaxLinesInUnion", "Union", "11", config)

        member this.MaxLinesInClass =
            SourceLengthRule(this.Name, "MaxLinesInClass", "Classes and interface", "12", config)

        override this.Analyse analysisArgs = 
            let mutable i = 0
            while i < analysisArgs.SyntaxArray.Length do
                match analysisArgs.SyntaxArray.[i].Actual with
                | AstNode.Expression(SynExpr.Lambda(_, _, _, _, range)) -> 
                    this.MaxLinesInLambdaFunction.Check(analysisArgs, i, range)
                | AstNode.Expression(SynExpr.MatchLambda(_, _, _, _, range)) -> 
                    this.MaxLinesInMatchLambdaFunction.Check(analysisArgs, i, range)
                | AstNode.Binding(SynBinding.Binding(_, _, _, _, _, _, valData, _, _, _, _, _) as binding) ->
                    match identifierTypeFromValData valData with
                    | Value -> this.MaxLinesInValue.Check(analysisArgs, i, binding.RangeOfBindingAndRhs)
                    | Function -> this.MaxLinesInFunction.Check(analysisArgs, i, binding.RangeOfBindingAndRhs)
                    | Member -> this.MaxLinesInMember.Check(analysisArgs, i, binding.RangeOfBindingAndRhs)
                    | Constructor -> this.MaxLinesInConstructor.Check(analysisArgs, i, binding.RangeOfBindingAndRhs)
                    | Property -> this.MaxLinesInProperty.Check(analysisArgs, i, binding.RangeOfBindingAndRhs)
                    | Other -> ()
                | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_, _, isModule, _, _, _, _, range)) when isModule -> 
                    this.MaxLinesInModule.Check(analysisArgs, i, range)
                | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, repr, _, range)) ->
                    match repr with
                    | SynTypeDefnRepr.Simple(simpleRepr, _) ->
                        match simpleRepr with
                        | SynTypeDefnSimpleRepr.Record(_) -> this.MaxLinesInRecord.Check(analysisArgs, i, range)
                        | SynTypeDefnSimpleRepr.Enum(_) -> this.MaxLinesInEnum.Check(analysisArgs, i, range)
                        | SynTypeDefnSimpleRepr.Union(_) -> this.MaxLinesInUnion.Check(analysisArgs, i, range)
                        | _ -> ()
                    | SynTypeDefnRepr.ObjectModel(_) -> this.MaxLinesInClass.Check(analysisArgs, i, range)
                    | SynTypeDefnRepr.Exception(_) -> ()
                | _ -> ()

                i <- i + 1