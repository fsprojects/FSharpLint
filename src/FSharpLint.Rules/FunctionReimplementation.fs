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
    open Microsoft.FSharp.Compiler.PrettyNaming
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.ExpressionUtilities

    [<Literal>]
    let AnalyserName = "FunctionReimplementation"
    (*
    let isRuleEnabled config ruleName =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome 

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
                    traverseNode node |> List.map (fun x -> x.AstNode) |> List.fold patternShadowsIdentifier false
                else 
                    found

        patternShadowsIdentifier false

    let rec expressionReferencesIdentifier (ident:Ident) expression =
        let rec expressionReferencesIdentifier isFound expr = 
            match (isFound, expr) with
            | NotFound, Expression(SynExpr.Ident(exprIdent))
            | NotFound, Expression(SynExpr.LongIdent(_, LongIdentWithDots.LongIdentWithDots(exprIdent::_, _), _, _))
                    when exprIdent.idText = ident.idText ->
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
            | NotFound, Expression(SynExpr.For(_, iteratorIdent, startExpr, _, finishExpr, _, _)) when iteratorIdent.idText = ident.idText -> 
                match expressionReferencesIdentifier NotFound (Expression(startExpr)) with
                | Found -> 
                    Found
                | _ -> 
                    match expressionReferencesIdentifier NotFound (Expression(finishExpr)) with
                    | Found -> Found
                    | _ -> NotFound
            | NotFound, Expression(SynExpr.ForEach(_, _, _, pattern, expr, _, _)) when patternShadowsIdentifier ident (Pattern(pattern)) -> 
                match expressionReferencesIdentifier NotFound (Expression(expr)) with
                | Found -> Found
                | _ -> NotFound
            | NotFound, Match(SynMatchClause.Clause(pattern, _, _, _, _)) when patternShadowsIdentifier ident (Pattern(pattern)) -> 
                NotFound
            | NotFound, expr ->
                match expr |> traverseNode |> List.map (fun x -> x.AstNode) |> List.fold expressionReferencesIdentifier NotFound with
                | Shadowed -> NotFound
                | isFound -> isFound
            | (isFound, _) -> isFound

        match expression |> Expression |> expressionReferencesIdentifier NotFound with
        | Found -> true
        | NotFound | Shadowed -> false

    let validateLambdaCannotBeReplacedWithComposition lambda range visitorInfo =
        let canBeReplacedWithFunctionComposition expression = 
            let removeLastElement = List.rev >> List.tail >> List.rev

            let getLastElement = List.rev >> List.head

            let rec lambdaArgumentIsLastApplicationInFunctionCalls expression lambdaArgument numFunctionCalls =
                let appliedValuesDoNotReferenceLambdaArgument appliedValues =
                    let lambdaArgumentNotReferenced = expressionReferencesIdentifier lambdaArgument >> not

                    List.isEmpty appliedValues |> not &&
                    removeLastElement appliedValues
                    |> List.forall lambdaArgumentNotReferenced

                match expression with
                | SynExpr.App(_, false, _, _, _) as nonInfixApp ->
                    match ExpressionUtilities.flattenFunctionApplication nonInfixApp with
                    | (SynExpr.Ident(_) | SynExpr.LongIdent(_))::appliedValues 
                            when appliedValuesDoNotReferenceLambdaArgument appliedValues -> 

                        match getLastElement appliedValues with
                        | SynExpr.Ident(lastArgument) when numFunctionCalls > 1 -> 
                            lastArgument.idText = lambdaArgument.idText
                        | SynExpr.App(_, false, _, _, _) as nextFunction ->
                            lambdaArgumentIsLastApplicationInFunctionCalls nextFunction lambdaArgument (numFunctionCalls + 1)
                        | _ -> false
                    | _ -> false
                | _ -> false

            match lambda.Arguments with
            | [singleParameter] -> 
                let paramIdent = getLambdaParamIdent singleParameter
                lambdaArgumentIsLastApplicationInFunctionCalls expression paramIdent 1
            | _ -> false
            
        if canBeReplacedWithFunctionComposition lambda.Body then
            Resources.GetString("RulesCanBeReplacedWithComposition") |> visitorInfo.PostError range 

    let validateLambdaIsNotPointless (checkFile:FSharpCheckFileResults option) lambda range visitorInfo =
        let isConstructor expr =
            let symbol = getSymbolFromIdent checkFile expr

            match symbol with
            | Some(symbol) -> 
                match symbol.Symbol with 
                | s when s.DisplayName = ".ctor" -> true
                | :? FSharpMemberOrFunctionOrValue as v when v.CompiledName = ".ctor" -> true
                | _ -> false
            | Some(_) | None -> false
        
        let rec isFunctionPointless expression = function
            | (parameter:Ident) :: parameters ->
                match expression with
                | SynExpr.App(_, _, expression, SynExpr.Ident(identifier), _)
                    when identifier.idText = parameter.idText ->
                        isFunctionPointless expression parameters
                | _ -> None
            | [] -> 
                match expression with
                | Identifier(ident, _) -> 
                    if visitorInfo.FSharpVersion.Major >= 4 || 
                       (not << isConstructor) expression then
                        Some(ident)
                    else
                        None
                | _ -> None

        let generateError (identifier:LongIdent) =
            let identifier = 
                identifier 
                    |> List.map (fun x -> DemangleOperatorName x.idText)
                    |> String.concat "."

            let errorFormatString = Resources.GetString("RulesReimplementsFunction")
            let error = System.String.Format(errorFormatString, identifier)
            visitorInfo.PostError range error

        isFunctionPointless lambda.Body (lambda.Arguments |> List.map getLambdaParamIdent)
        |> Option.iter generateError

    let visitor visitorInfo checkFile (syntaxArray:AbstractSyntaxArray.Node []) _ = 
        let mutable i = 0
        while i < syntaxArray.Length do
            match syntaxArray.[i].Actual with
            | AstNode.Expression(SynExpr.Lambda(_)) as lambda -> 
                match lambda with
                | Lambda(lambda, range) -> 
                    if (not << List.isEmpty) lambda.Arguments then
                        if isRuleEnabled visitorInfo.Config "ReimplementsFunction" then
                            validateLambdaIsNotPointless checkFile lambda range visitorInfo
                    
                        if isRuleEnabled visitorInfo.Config "CanBeReplacedWithComposition" then
                            validateLambdaCannotBeReplacedWithComposition lambda range visitorInfo
                | _ -> ()
            | _ -> ()

            i <- i + 1*)
    ()