module FSharpLint.Rules.CanBeReplacedWithComposition

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.ExpressionUtilities

let private validateLambdaCannotBeReplacedWithComposition _ lambda range =
    let canBeReplacedWithFunctionComposition expression =
        let getLastElement = List.rev >> List.tryHead

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

                    let lastElement = getLastElement appliedValues
                    match lastElement with
                    | Some element ->
                        match element with
                        | SynExpr.Ident(lastArgument) when numFunctionCalls > 1 ->
                            lastArgument.idText = lambdaArgument.idText
                        | SynExpr.App(_, false, _, _, _) as nextFunction ->
                            lambdaArgumentIsLastApplicationInFunctionCalls nextFunction lambdaArgument (numFunctionCalls + 1)
                        | _ -> false
                    | None -> failwith "There's no last element."
                | _ -> false
            | _ -> false

        match lambda.Arguments with
        | [singleParameter] ->
            Helper.FunctionReimplementation.getLambdaParamIdent singleParameter
            |> Option.exists (fun paramIdent -> lambdaArgumentIsLastApplicationInFunctionCalls expression paramIdent 1)
        | _ -> false

    if canBeReplacedWithFunctionComposition lambda.Body then
        Array.singleton
            {
                Range = range
                Message = Resources.GetString("RulesCanBeReplacedWithComposition")
                SuggestedFix = None
                TypeChecks = List.Empty
            }
    else
        Array.empty

let runner (args:AstNodeRuleParams) =
    Helper.FunctionReimplementation.checkLambda args validateLambdaCannotBeReplacedWithComposition

let rule =
    AstNodeRule
        {
            Name = "CanBeReplacedWithComposition"
            Identifier = Identifiers.CanBeReplacedWithComposition
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
