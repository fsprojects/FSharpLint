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

    let checkFailwith visitorInfo flattenedExpression range =
        if "FailwithWithSingleArgument" |> isRuleEnabled visitorInfo.Config then
            match flattenedExpression with
                | SynExpr.Ident(ident)::arguments when List.length arguments > 1 && ident.idText = "failwith" ->
                    visitorInfo.PostError range (FSharpLint.Framework.Resources.GetString("RulesFailwithWithSingleArgument"))
                | _ -> ()

    let checkRange visitorInfo flattenedExpression range =
        if "RaiseWithSingleArgument" |> isRuleEnabled visitorInfo.Config then
            match flattenedExpression with
                | SynExpr.Ident(ident)::arguments when List.length arguments > 1 && ident.idText = "raise" ->
                    visitorInfo.PostError range (FSharpLint.Framework.Resources.GetString("RulesRaiseWithSingleArgument"))
                | _ -> ()

    let checkFailwithf visitorInfo flattenedExpression range =
        if "FailwithfWithArgumentsMatchingFormatString" |> isRuleEnabled visitorInfo.Config then
            match flattenedExpression with
                | SynExpr.Ident(ident)::SynExpr.Const(SynConst.String(formatString, _), _)::arguments 
                    when ident.idText = "failwithf" && List.length arguments = formatString.Replace("%%", "").Split('%').Length ->
                        visitorInfo.PostError range (FSharpLint.Framework.Resources.GetString("RulesFailwithfWithArgumentsMatchingFormatString"))
                | _ -> ()
    
    let visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.Expression(SynExpr.App(_, _, _, _, range) as expr) -> 
                let flattenedExpression = FSharpLint.Framework.ExpressionUtilities.flattenFunctionApplication expr

                checkFailwith visitorInfo flattenedExpression range
                checkFailwithf visitorInfo flattenedExpression range
                checkRange visitorInfo flattenedExpression range
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