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

    let private raiseFunctionHasTooManyArgs identifier = function
        | SynExpr.Ident(ident)::arguments when List.length arguments > 1 && ident.idText = identifier ->
            true
        | _ -> false

    let private raiseFunctionWithFormatStringHasTooManyArgs identifier = function
        | SynExpr.Ident(ident)::SynExpr.Const(SynConst.String(formatString, _), _)::arguments 
            when ident.idText = identifier && List.length arguments = formatString.Replace("%%", "").Split('%').Length ->
                true
        | _ -> false

    type private DoesHaveFormatString =
        | HasFormatString
        | NoFormatString

    type private CheckFunctionInfo =
        {
            RuleName: string
            FunctionIdentifier: string
            ResourceStringName: string
            FlattenedExpression: SynExpr list
            Range: range
            DoesHaveFormatString: DoesHaveFormatString
        }

    let private checkFunction visitorInfo checkFunctionInfo =
        let ruleIsEnabled = checkFunctionInfo.RuleName |> isRuleEnabled visitorInfo.Config

        let hasTooManyArguments () =
            let checkArguments = 
                match checkFunctionInfo.DoesHaveFormatString with
                    | HasFormatString -> raiseFunctionWithFormatStringHasTooManyArgs
                    | NoFormatString -> raiseFunctionHasTooManyArgs

            checkArguments checkFunctionInfo.FunctionIdentifier checkFunctionInfo.FlattenedExpression

        if ruleIsEnabled && hasTooManyArguments() then
            let error = FSharpLint.Framework.Resources.GetString checkFunctionInfo.ResourceStringName

            visitorInfo.PostError checkFunctionInfo.Range error

    let checkFailwith visitorInfo flattenedExpression range =
        checkFunction visitorInfo 
            {
                RuleName = "FailwithWithSingleArgument"
                FunctionIdentifier = "failwith"
                ResourceStringName = "RulesFailwithWithSingleArgument"
                FlattenedExpression = flattenedExpression
                Range = range
                DoesHaveFormatString = NoFormatString
            }

    let checkRange visitorInfo flattenedExpression range =
        checkFunction visitorInfo 
            {
                RuleName = "RaiseWithSingleArgument"
                FunctionIdentifier = "raise"
                ResourceStringName = "RulesRaiseWithSingleArgument"
                FlattenedExpression = flattenedExpression
                Range = range
                DoesHaveFormatString = NoFormatString
            }

    let checkNullArg visitorInfo flattenedExpression range =
        checkFunction visitorInfo 
            {
                RuleName = "NullArgWithSingleArgument"
                FunctionIdentifier = "nullArg"
                ResourceStringName = "RulesNullArgWithSingleArgument"
                FlattenedExpression = flattenedExpression
                Range = range
                DoesHaveFormatString = NoFormatString
            }

    let checkInvalidOp visitorInfo flattenedExpression range =
        checkFunction visitorInfo 
            {
                RuleName = "InvalidOpWithSingleArgument"
                FunctionIdentifier = "invalidOp"
                ResourceStringName = "RulesInvalidOpWithSingleArgument"
                FlattenedExpression = flattenedExpression
                Range = range
                DoesHaveFormatString = NoFormatString
            }

    let checkInvalidArg visitorInfo flattenedExpression range =
        checkFunction visitorInfo 
            {
                RuleName = "InvalidArgWithArgumentsMatchingFormatString"
                FunctionIdentifier = "invalidArg"
                ResourceStringName = "RulesInvalidArgWithArgumentsMatchingFormatString"
                FlattenedExpression = flattenedExpression
                Range = range
                DoesHaveFormatString = HasFormatString
            }

    let checkFailwithf visitorInfo flattenedExpression range =
        checkFunction visitorInfo 
            {
                RuleName = "FailwithfWithArgumentsMatchingFormatString"
                FunctionIdentifier = "failwithf"
                ResourceStringName = "RulesFailwithfWithArgumentsMatchingFormatString"
                FlattenedExpression = flattenedExpression
                Range = range
                DoesHaveFormatString = HasFormatString
            }
    
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