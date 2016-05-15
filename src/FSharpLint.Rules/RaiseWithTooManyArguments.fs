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
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "RaiseWithTooManyArguments"

    let (|RaiseWithTooManyArgs|_|) identifier maxArgs = function
        | SynExpr.Ident(ident)::arguments when List.length arguments > maxArgs && ident.idText = identifier ->
            Some()
        | _ -> None

    let (|RaiseWithFormatStringTooManyArgs|_|) identifier = function
        | SynExpr.Ident(ident)::SynExpr.Const(SynConst.String(formatString, _), _)::arguments 
            when ident.idText = identifier && List.length arguments = formatString.Replace("%%", "").Split('%').Length ->
                Some()
        | _ -> None

    type private CheckFunctionInfo =
        { RuleName: string
          ResourceStringName: string
          Range: range }

    let private checkFunction visitorInfo checkFunctionInfo hasTooManyArguments isEnabled =
        let ruleIsEnabled = isEnabled checkFunctionInfo.RuleName

        if ruleIsEnabled && hasTooManyArguments() then
            let error = Resources.GetString checkFunctionInfo.ResourceStringName

            visitorInfo.PostError checkFunctionInfo.Range error

    let checkFailwith visitorInfo flattenedExpression range isEnabled =
        let hasTooManyArguments () =
            match flattenedExpression with
            | RaiseWithTooManyArgs "failwith" 1 -> true
            | _ -> false

        checkFunction visitorInfo
            { RuleName = "FailwithWithSingleArgument"
              ResourceStringName = "RulesFailwithWithSingleArgument"
              Range = range } hasTooManyArguments isEnabled

    let checkRange visitorInfo flattenedExpression range isEnabled =
        let hasTooManyArguments () =
            match flattenedExpression with
            | RaiseWithTooManyArgs "raise" 1 -> true
            | _ -> false

        checkFunction visitorInfo
            { RuleName = "RaiseWithSingleArgument"
              ResourceStringName = "RulesRaiseWithSingleArgument"
              Range = range } hasTooManyArguments isEnabled

    let checkNullArg visitorInfo flattenedExpression range isEnabled =
        let hasTooManyArguments () =
            match flattenedExpression with
            | RaiseWithTooManyArgs "nullArg" 1 -> true
            | _ -> false

        checkFunction visitorInfo
            { RuleName = "NullArgWithSingleArgument"
              ResourceStringName = "RulesNullArgWithSingleArgument"
              Range = range } hasTooManyArguments isEnabled

    let checkInvalidOp visitorInfo flattenedExpression range isEnabled =
        let hasTooManyArguments () =
            match flattenedExpression with
            | RaiseWithTooManyArgs "invalidOp" 1 -> true
            | _ -> false

        checkFunction visitorInfo
            { RuleName = "InvalidOpWithSingleArgument"
              ResourceStringName = "RulesInvalidOpWithSingleArgument"
              Range = range } hasTooManyArguments isEnabled

    let checkInvalidArg visitorInfo flattenedExpression range isEnabled =
        let hasTooManyArguments () =
            match flattenedExpression with
            | RaiseWithTooManyArgs "invalidArg" 2 -> true
            | _ -> false

        checkFunction visitorInfo
            { RuleName = "InvalidArgWithTwoArguments"
              ResourceStringName = "RulesInvalidArgWithTwoArguments"
              Range = range } hasTooManyArguments isEnabled

    let checkFailwithf visitorInfo flattenedExpression range isEnabled =
        let hasTooManyArguments () =
            match flattenedExpression with
            | RaiseWithFormatStringTooManyArgs "failwithf" -> true
            | _ -> false

        checkFunction visitorInfo
            { RuleName = "FailwithfWithArgumentsMatchingFormatString"
              ResourceStringName = "RulesFailwithfWithArgumentsMatchingFormatString"
              Range = range } hasTooManyArguments isEnabled

    let visitor visitorInfo _ syntaxArray skipArray = 
        let isEnabled i ruleName =
            match isRuleEnabled visitorInfo.Config AnalyserName ruleName with
            | Some(_) -> 
                let isSuppressed =
                    AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
                    |> List.exists (List.exists (fun (l, _) -> l.Category = AnalyserName && l.Rule = ruleName))
                not isSuppressed
            | None -> false

        let mutable i = 0
        while i < syntaxArray.Length do
            match syntaxArray.[i].Actual with
            | AstNode.Expression(SynExpr.App(_, false, _, _, _)) as expr -> 
                match expr with
                | FuncApp(flattenedExpression, range) -> 
                    checkFailwith visitorInfo flattenedExpression range (isEnabled i)
                    checkRange visitorInfo flattenedExpression range (isEnabled i)
                    checkNullArg visitorInfo flattenedExpression range (isEnabled i)
                    checkInvalidOp visitorInfo flattenedExpression range (isEnabled i)
                    checkInvalidArg visitorInfo flattenedExpression range (isEnabled i)
                    checkFailwithf visitorInfo flattenedExpression range (isEnabled i)
                | _ -> ()
            | _ -> ()

            i <- i + 1