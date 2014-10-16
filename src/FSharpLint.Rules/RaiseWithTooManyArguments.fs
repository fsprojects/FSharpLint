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

module RaiseWithTooManyArguments =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors

    [<Literal>]
    let AnalyserName = "FSharpLint.RaiseWithTooManyArguments"

    let isRuleEnabled config ruleName =
        match isRuleEnabled config AnalyserName ruleName with
            | Some(_) -> true
            | None -> false

    let (|RaiseWithTooManyArgs|_|) identifier maxArgs = function
        | SynExpr.Ident(ident)::arguments when List.length arguments > maxArgs && ident.idText = identifier ->
            Some()
        | _ -> None

    let (|RaiseWithFormatStringTooManyArgs|_|) identifier = function
        | SynExpr.Ident(ident)::SynExpr.Const(SynConst.String(formatString, _), _)::arguments 
            when ident.idText = identifier && List.length arguments = formatString.Replace("%%", "").Split('%').Length ->
                Some()
        | _ -> None

    type private DoesHaveFormatString =
        | HasFormatString
        | NoFormatString

    type private CheckFunctionInfo =
        {
            RuleName: string
            ResourceStringName: string
            Range: range
        }

    let private checkFunction visitorInfo checkFunctionInfo hasTooManyArguments =
        let ruleIsEnabled = checkFunctionInfo.RuleName |> isRuleEnabled visitorInfo.Config

        if ruleIsEnabled && hasTooManyArguments() then
            let error = FSharpLint.Framework.Resources.GetString checkFunctionInfo.ResourceStringName

            visitorInfo.PostError checkFunctionInfo.Range error

    let checkFailwith visitorInfo flattenedExpression range =
        let hasTooManyArguments () =
            match flattenedExpression with
                | RaiseWithTooManyArgs "failwith" 1 -> true
                | _ -> false

        checkFunction visitorInfo 
            {
                RuleName = "FailwithWithSingleArgument"
                ResourceStringName = "RulesFailwithWithSingleArgument"
                Range = range
            } hasTooManyArguments

    let checkRange visitorInfo flattenedExpression range =
        let hasTooManyArguments () =
            match flattenedExpression with
                | RaiseWithTooManyArgs "raise" 1 -> true
                | _ -> false

        checkFunction visitorInfo 
            {
                RuleName = "RaiseWithSingleArgument"
                ResourceStringName = "RulesRaiseWithSingleArgument"
                Range = range
            } hasTooManyArguments

    let checkNullArg visitorInfo flattenedExpression range =
        let hasTooManyArguments () =
            match flattenedExpression with
                | RaiseWithTooManyArgs "nullArg" 1 -> true
                | _ -> false

        checkFunction visitorInfo 
            {
                RuleName = "NullArgWithSingleArgument"
                ResourceStringName = "RulesNullArgWithSingleArgument"
                Range = range
            } hasTooManyArguments

    let checkInvalidOp visitorInfo flattenedExpression range =
        let hasTooManyArguments () =
            match flattenedExpression with
                | RaiseWithTooManyArgs "invalidOp" 1 -> true
                | _ -> false

        checkFunction visitorInfo 
            {
                RuleName = "InvalidOpWithSingleArgument"
                ResourceStringName = "RulesInvalidOpWithSingleArgument"
                Range = range
            } hasTooManyArguments

    let checkInvalidArg visitorInfo flattenedExpression range =
        let hasTooManyArguments () =
            match flattenedExpression with
                | RaiseWithTooManyArgs "invalidArg" 2 -> true
                | _ -> false

        checkFunction visitorInfo 
            {
                RuleName = "InvalidArgWithTwoArguments"
                ResourceStringName = "RulesInvalidArgWithTwoArguments"
                Range = range
            } hasTooManyArguments

    let checkFailwithf visitorInfo flattenedExpression range =
        let hasTooManyArguments () =
            match flattenedExpression with
                | RaiseWithFormatStringTooManyArgs "failwithf" -> true
                | _ -> false

        checkFunction visitorInfo 
            {
                RuleName = "FailwithfWithArgumentsMatchingFormatString"
                ResourceStringName = "RulesFailwithfWithArgumentsMatchingFormatString"
                Range = range
            } hasTooManyArguments
    
    let visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.Expression(SynExpr.App(_, _, _, _, range) as expr) -> 
                let flattenedExpression = FSharpLint.Framework.ExpressionUtilities.flattenFunctionApplication expr
                
                checkFailwith visitorInfo flattenedExpression range
                checkRange visitorInfo flattenedExpression range
                checkNullArg visitorInfo flattenedExpression range
                checkInvalidOp visitorInfo flattenedExpression range
                checkInvalidArg visitorInfo flattenedExpression range
                checkFailwithf visitorInfo flattenedExpression range
            | _ -> ()

        Continue

    type RegisterBindingVisitor() = 
        let plugin =
            {
                Name = AnalyserName
                Visitor = Ast(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin