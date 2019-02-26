namespace FSharpLint.Rules

/// Checks a lambda function is not simply an 'abbreviation' of another function.
/// For example it will warn when it finds a lambda such as: fun a b -> a * b as it is exactly the same as (*).
module FunctionReimplementation =
    
    open System
    open FSharp.Compiler.Ast
    open FSharp.Compiler.PrettyNaming
    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.ExpressionUtilities

    [<Literal>]
    let AnalyserName = "FunctionReimplementation"
    
    let private isRuleEnabled config ruleName =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome 

    let rec private simplePatternsLength = function
        | SynSimplePats.SimplePats(patterns, _) -> 
            List.length patterns
        | SynSimplePats.Typed(simplePatterns, _, _) -> 
            simplePatternsLength simplePatterns

    let rec private getLambdaParamIdent = function
        | SynSimplePats.SimplePats([pattern], _) -> 
            let rec getIdent = function
                | SynSimplePat.Id(ident, _, _, _, _, _) -> ident
                | SynSimplePat.Typed(simplePattern, _, _)
                | SynSimplePat.Attrib(simplePattern, _, _) ->
                    getIdent simplePattern

            getIdent pattern |> Some
        | SynSimplePats.SimplePats(_) -> None
        | SynSimplePats.Typed(simplePatterns, _, _) -> 
            getLambdaParamIdent simplePatterns

    let private validateLambdaCannotBeReplacedWithComposition lambda range analyserInfo =
        let canBeReplacedWithFunctionComposition expression = 
            let getLastElement = List.rev >> List.head

            let rec lambdaArgumentIsLastApplicationInFunctionCalls expression (lambdaArgument:Ident) numFunctionCalls =
                let rec appliedValuesAreConstants appliedValues =
                    match appliedValues with
                    | (SynExpr.Const(_)| SynExpr.Null(_))::rest -> appliedValuesAreConstants rest
                    | [SynExpr.App(_) | SynExpr.Ident(_)] -> true
                    | _ -> false

                match AstNode.Expression expression with
                | FuncApp(exprs, _) ->
                    match List.map removeParens exprs with
                    | (SynExpr.Ident(_) | SynExpr.LongIdent(_))::appliedValues
                            when appliedValuesAreConstants appliedValues -> 

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
                getLambdaParamIdent singleParameter
                |> Option.exists (fun paramIdent -> lambdaArgumentIsLastApplicationInFunctionCalls expression paramIdent 1)
            | _ -> false
            
        if canBeReplacedWithFunctionComposition lambda.Body then
            analyserInfo.Suggest
                { Range = range 
                  Message = Resources.GetString("RulesCanBeReplacedWithComposition")
                  SuggestedFix = None
                  TypeChecks = [] }

    let private validateLambdaIsNotPointless lambda range (analyserInfo:AnalyserInfo) =        
        let rec isFunctionPointless expression = function
            | Some(parameter:Ident) :: parameters ->
                match expression with
                | SynExpr.App(_, _, expression, SynExpr.Ident(identifier), _)
                    when identifier.idText = parameter.idText ->
                        isFunctionPointless expression parameters
                | _ -> None
            | None :: _ -> None
            | [] -> 
                match expression with
                | Identifier(ident, _) -> Some(ident)
                | _ -> None

        let generateError (identifier:LongIdent) =
            let identifier = 
                identifier 
                |> List.map (fun x -> DemangleOperatorName x.idText)
                |> String.concat "."

            let suggestedFix = lazy(
                analyserInfo.TryFindTextOfRange range
                |> Option.map (fun fromText -> { FromText = fromText; FromRange = range; ToText = identifier }))

            analyserInfo.Suggest 
                { Range = range
                  Message = String.Format(Resources.GetString("RulesReimplementsFunction"), identifier)
                  SuggestedFix = Some suggestedFix
                  TypeChecks = [] }

        let argumentsAsIdentifiers = lambda.Arguments |> List.map getLambdaParamIdent |> List.rev

        isFunctionPointless lambda.Body argumentsAsIdentifiers
        |> Option.iter generateError
        
    let analyser (args: AnalyserArgs) : unit = 
        let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

        let isSuppressed i ruleName = 
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName

        let isEnabled i ruleName = isRuleEnabled args.Info.Config ruleName && not (isSuppressed i ruleName)
        
        for i = 0 to syntaxArray.Length - 1 do
            match syntaxArray.[i].Actual with
            | AstNode.Expression(SynExpr.Lambda(_)) as lambda -> 
                match lambda with
                | Lambda(lambda, range) -> 
                    if (not << List.isEmpty) lambda.Arguments then
                        if isEnabled i "ReimplementsFunction" then
                            validateLambdaIsNotPointless lambda range args.Info
                    
                        if isEnabled i "CanBeReplacedWithComposition" then
                            validateLambdaCannotBeReplacedWithComposition lambda range args.Info
                | _ -> ()
            | _ -> ()