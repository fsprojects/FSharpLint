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
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors

    [<Literal>]
    let AnalyserName = "FSharpLint.FunctionReimplementation"

    let isAnalyserEnabled config =
        isAnalyserEnabled config AnalyserName |> Option.isSome 

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
        
    type IdentifierInExpression =
        | Shadowed
        | Found
        | NotFound

    let lambdaArgumentsShadowIdent (ident:Ident) = function
        | SynExpr.Lambda(_) as lambda -> 
            let arguments, _ = getLambdaParametersAndExpression lambda
            arguments |> List.exists (fun argumentIdent -> argumentIdent.idText = ident.idText)
        | _ -> false

    let patternShadowsIdentifier (identifier:Ident) =
        let rec patternShadowsIdentifier found = function
            | Pattern(SynPat.Named(_, ident, _, _, _) | SynPat.OptionalVal(ident, _)) when ident.idText = identifier.idText -> true
            | node -> 
                if not found then
                    traverseNode node |> List.fold patternShadowsIdentifier false
                else 
                    found

        patternShadowsIdentifier false

    let rec expressionReferencesIdentifier (ident:Ident) expression =
        let rec expressionReferencesIdentifier isFound expr = 
            match (isFound, expr) with
                | NotFound, Expression(SynExpr.Ident(exprIdent)) when exprIdent.idText = ident.idText ->
                    Found
                | NotFound, Expression(SynExpr.Lambda(_) as lambda) when lambdaArgumentsShadowIdent ident lambda -> 
                    NotFound
                | NotFound, Expression(SynExpr.LetOrUseBang(_, _, _, pattern, expression, bodyExpr, _))
                | NotFound, Expression(SynExpr.LetOrUse(_, _, [SynBinding.Binding(_, _, _, _, _, _, _, pattern, _, expression, _, _)], bodyExpr, _)) -> 
                    match expressionReferencesIdentifier NotFound (Expression(expression)) with
                        | Found -> 
                            Found
                        | NotFound | Shadowed when patternShadowsIdentifier ident (Pattern(pattern)) -> 
                            Shadowed
                        | _ -> 
                            match expressionReferencesIdentifier NotFound (Expression(bodyExpr)) with
                                | Shadowed -> NotFound
                                | isFound -> isFound
                | NotFound, Match(SynMatchClause.Clause(pattern, whenClause, expr, _, _)) when patternShadowsIdentifier ident (Pattern(pattern)) -> 
                    NotFound
                | NotFound, expr ->
                    match expr |> traverseNode |> List.fold expressionReferencesIdentifier NotFound with
                        | Shadowed -> NotFound
                        | isFound -> isFound
                | (isFound, _) -> isFound

        match expression |> Expression |> expressionReferencesIdentifier NotFound with
            | Found -> true
            | NotFound | Shadowed -> false

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

        let canBeReplacedWithFunctionComposition expression = 
            let removeLastElement = List.rev >> List.tail >> List.rev

            let getLastElement = List.rev >> List.head

            let rec lambdaArgumentIsLastApplicationInFunctionCalls expression lambdaArgument =
                let appliedValuesDoNotReferenceLambdaArgument appliedValues =
                    List.length appliedValues > 0 &&
                    removeLastElement appliedValues
                         |> (List.forall ((expressionReferencesIdentifier lambdaArgument) >> not))

                match ExpressionUtilities.flattenFunctionApplication expression with
                    | (SynExpr.Ident(_) | SynExpr.LongIdent(_))::appliedValues 
                            when appliedValuesDoNotReferenceLambdaArgument appliedValues -> 

                        match getLastElement appliedValues with
                            | SynExpr.Ident(lastArgument) -> 
                                lastArgument.idText = lambdaArgument.idText
                            | SynExpr.App(_) as nextFunction ->
                                lambdaArgumentIsLastApplicationInFunctionCalls nextFunction lambdaArgument
                            | _ -> 
                                false
                    | _ -> false

            match parameters with
                | [singleParameter] -> 
                    lambdaArgumentIsLastApplicationInFunctionCalls expression singleParameter
                | _ -> false
            
        if canBeReplacedWithFunctionComposition expression then
            Resources.GetString("RulesFunctionCompositionError") |> visitorInfo.PostError range 

        isFunctionPointless expression parameters 
            |> Option.iter (fun identifier ->
                let errorFormatString = Resources.GetString("RulesFunctionReimplementationError")
                let error = System.String.Format(errorFormatString, identifier.idText)
                visitorInfo.PostError range error)
    
    let visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.Expression(SynExpr.Lambda(_) as lambda) when isAnalyserEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName) |> not ->
                let (parameters, expression) = getLambdaParametersAndExpression lambda

                if List.length parameters > 0 then
                    validateFunctionIsNotPointless parameters expression lambda.Range visitorInfo
            | _ -> ()

        Continue

    type RegisterFunctionReimplementationVisitor() = 
        let plugin =
            {
                Name = AnalyserName
                Visitor = Ast(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin