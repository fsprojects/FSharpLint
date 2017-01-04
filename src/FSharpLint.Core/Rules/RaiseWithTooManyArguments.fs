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

module RaiseWithTooManyArguments =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "RaiseWithTooManyArguments"

    let private (|RaiseWithTooManyArgs|_|) identifier maxArgs = function
        | SynExpr.Ident(ident)::arguments when List.length arguments > maxArgs && ident.idText = identifier ->
            Some()
        | _ -> None

    let private (|RaiseWithFormatStringTooManyArgs|_|) identifier = function
        | SynExpr.Ident(ident)::SynExpr.Const(SynConst.String(formatString, _), _)::arguments 
            when ident.idText = identifier && List.length arguments = formatString.Replace("%%", "").Split('%').Length ->
                Some()
        | _ -> None
            
    let analyser (args: AnalyserArgs) : unit = 
        let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

        let isEnabled i ruleName =
            match isRuleEnabled args.Info.Config AnalyserName ruleName with
            | Some(_) -> 
                let isSuppressed =
                    AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
                    |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
                not isSuppressed
            | None -> false
            
        let postError ruleName errorName range isEnabled =
            if isEnabled ruleName then
                args.Info.Suggest { Range = range; Message = Resources.GetString errorName; SuggestedFix = None }

        let mutable i = 0
        while i < syntaxArray.Length do
            match syntaxArray.[i].Actual with
            | AstNode.Expression(SynExpr.App(_, false, _, _, _)) as expr -> 
                match expr with
                | FuncApp(expressions, range) -> 
                    match expressions with
                    | RaiseWithTooManyArgs "failwith" 1 ->
                        postError "FailwithWithSingleArgument" "FailwithWithSingleArgument" range (isEnabled i)
                    | RaiseWithTooManyArgs "raise" 1 -> 
                        postError "RaiseWithSingleArgument" "RulesRaiseWithSingleArgument" range (isEnabled i)
                    | RaiseWithTooManyArgs "nullArg" 1 -> 
                        postError "NullArgWithSingleArgument" "RulesNullArgWithSingleArgument" range (isEnabled i)
                    | RaiseWithTooManyArgs "invalidOp" 1 -> 
                        postError "InvalidOpWithSingleArgument" "RulesInvalidOpWithSingleArgument" range (isEnabled i)
                    | RaiseWithTooManyArgs "invalidArg" 2 -> 
                        postError "InvalidArgWithTwoArguments" "InvalidArgWithTwoArguments" range (isEnabled i)
                    | RaiseWithFormatStringTooManyArgs "failwithf" -> 
                        postError "FailwithfWithArgumentsMatchingFormatString" "FailwithfWithArgumentsMatchingFormatString" range (isEnabled i)
                    | _ -> ()
                | _ -> ()
            | _ -> ()

            i <- i + 1