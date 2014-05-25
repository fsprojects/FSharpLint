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

/// Checks a lambda function is not simply an 'abbreviation' of another function.
/// For example it will warn when it finds a lambda such as: fun a b -> a * b as it is exactly the same as (*).
module FunctionReimplementation =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadAnalysers

    [<Literal>]
    let AnalyserName = "FSharpLint.FunctionReimplementation"

    let isAnalyserEnabled config =
        (isAnalyserEnabled config AnalyserName).IsSome

    let validateFunctionIsNotPointless parameters expression range visitorInfo =
        let rec isFunctionPointless expression = function
            | (parameter:Ident) :: parameters ->
                match expression with
                    | SynExpr.App(_, _, expression, SynExpr.Ident(identifier), _)
                        when identifier.idText = parameter.idText ->
                            isFunctionPointless expression parameters
                    | _ -> None
            | [] -> 
                match expression with
                    | SynExpr.Ident(identifier) -> Some(identifier)
                    | _ -> None

        isFunctionPointless expression parameters 
            |> Option.iter (fun identifier ->
                let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesFunctionReimplementationError")
                let error = System.String.Format(errorFormatString, identifier.idText)
                visitorInfo.PostError range error)

    let rec simplePatternsLength = function
        | SynSimplePats.SimplePats(patterns, _) -> 
            List.length patterns
        | SynSimplePats.Typed(simplePatterns, _, _) -> 
            simplePatternsLength simplePatterns

    let rec private getLambdaParamIdent = function
        | SynSimplePats.SimplePats(patterns, _) -> 
            let rec getIdent = function
                | SynSimplePat.Id(ident, _, _, _, _, _) -> ident
                | SynSimplePat.Typed(simplePattern, _, _)
                | SynSimplePat.Attrib(simplePattern, _, _) ->
                    getIdent simplePattern

            getIdent patterns.Head
        | SynSimplePats.Typed(simplePatterns, _, _) -> 
            getLambdaParamIdent simplePatterns

    let getLambdaParametersAndExpression lambda =
        let rec getLambdaParametersAndExpression parameters = function
            | SynExpr.Lambda(_, _, parameter, expression, _) when simplePatternsLength parameter > 0 ->
                let ident = getLambdaParamIdent parameter
                getLambdaParametersAndExpression (ident :: parameters) expression
            // Unit argument
            | SynExpr.Lambda(_, _, _, expression, _) -> ([], expression)
            | expression -> (parameters, expression)

        getLambdaParametersAndExpression [] lambda
    
    let visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.Expression(expression) when isAnalyserEnabled visitorInfo.Config ->
                match expression with
                    | SynExpr.Lambda(_) as lambda -> 
                        let (parameters, expression) = getLambdaParametersAndExpression lambda

                        if List.length parameters > 0 then
                            validateFunctionIsNotPointless parameters expression lambda.Range visitorInfo
                    | _ -> ()
            | _ -> ()

        Continue

    type RegisterFunctionReimplementationAnalyser() = 
        let plugin =
            {
                Name = AnalyserName
                Analyser = Ast(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin