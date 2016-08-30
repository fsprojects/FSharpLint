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
    
    module Analysis =
        type IRaiseWithTooManyArguementsAnalyser =
            abstract member FailwithWithSingleArgument: Rule
            abstract member RaiseWithSingleArgument: Rule
            abstract member NullArgWithSingleArgument: Rule
            abstract member InvalidOpWithSingleArgument: Rule
            abstract member InvalidArgWithTwoArguments: Rule
            abstract member FailwithfWithArgumentsMatchingFormatString: Rule

        let private (|RaiseWithTooManyArgs|_|) identifier maxArgs = function
            | SynExpr.Ident(ident)::arguments when List.length arguments > maxArgs && ident.idText = identifier ->
                Some()
            | _ -> None

        let private (|RaiseWithFormatStringTooManyArgs|_|) identifier = function
            | SynExpr.Ident(ident)::SynExpr.Const(SynConst.String(formatString, _), _)::arguments 
                when ident.idText = identifier && List.length arguments = formatString.Replace("%%", "").Split('%').Length ->
                    Some()
            | _ -> None
            
        let analyse (analyser:IRaiseWithTooManyArguementsAnalyser) analysisArgs =             
            let mutable i = 0
            while i < analysisArgs.SyntaxArray.Length do
                match analysisArgs.SyntaxArray.[i].Actual with
                | AstNode.Expression(SynExpr.App(_, false, _, _, _)) as expr -> 
                    match expr with
                    | FuncApp(expressions, range) -> 
                        let maybeRuleBroken =
                            match expressions with
                            | RaiseWithTooManyArgs "failwith" 1 ->
                                Some analyser.FailwithWithSingleArgument
                            | RaiseWithTooManyArgs "raise" 1 -> 
                                Some analyser.RaiseWithSingleArgument
                            | RaiseWithTooManyArgs "nullArg" 1 -> 
                                Some analyser.NullArgWithSingleArgument
                            | RaiseWithTooManyArgs "invalidOp" 1 -> 
                                Some analyser.InvalidOpWithSingleArgument
                            | RaiseWithTooManyArgs "invalidArg" 2 -> 
                                Some analyser.InvalidArgWithTwoArguments
                            | RaiseWithFormatStringTooManyArgs "failwithf" -> 
                                Some analyser.FailwithfWithArgumentsMatchingFormatString
                            | _ -> None

                        match maybeRuleBroken with
                        | Some(ruleBroken) -> ruleBroken.MessageFormat() |> analysisArgs.Context.PostError range
                        | None -> ()
                    | _ -> ()
                | _ -> ()

                i <- i + 1

    [<Sealed>]
    type RaiseWithTooManyArguementsAnalyser(config) =
        inherit Analyser.Analyser(name = "Binding", code = "1", config = config)

        interface Analysis.IRaiseWithTooManyArguementsAnalyser with
            member this.FailwithWithSingleArgument = 
                this.Rule(ruleName = "FailwithWithSingleArgument", code = "1", ruleConfig = config)

            member this.RaiseWithSingleArgument = 
                this.Rule(ruleName = "RaiseWithSingleArgument", code = "2", ruleConfig = config)

            member this.NullArgWithSingleArgument = 
                this.Rule(ruleName = "NullArgWithSingleArgument", code = "3", ruleConfig = config)

            member this.InvalidOpWithSingleArgument = 
                this.Rule(ruleName = "InvalidOpWithSingleArgument", code = "4", ruleConfig = config)

            member this.InvalidArgWithTwoArguments = 
                this.Rule(ruleName = "InvalidArgWithTwoArguments", code = "5", ruleConfig = config)

            member this.FailwithfWithArgumentsMatchingFormatString = 
                this.Rule(ruleName = "FailwithfWithArgumentsMatchingFormatString", code = "6", ruleConfig = config)

        override this.Analyse analysisArgs = 
            Analysis.analyse this analysisArgs