module FSharpLint.Rules.CanBeReplacedWithComposition

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.ExpressionUtilities

let private validateLambdaCannotBeReplacedWithComposition fileContents _ lambda range =
    let tryReplaceWithFunctionComposition expression =
        let getLastElement = List.rev >> List.head

        let rec lambdaArgumentIsLastApplicationInFunctionCalls expression (lambdaArgument:Ident) (calledFunctionIdents: List<string>) =
            let rec appliedValuesAreConstants appliedValues =
                match appliedValues with
                | (SynExpr.Const(_)| SynExpr.Null(_))::rest -> appliedValuesAreConstants rest
                | [SynExpr.App(_) | SynExpr.Ident(_)] -> true
                | _ -> false

            match AstNode.Expression expression with
            | FuncApp(exprs, _) ->
                match List.map removeParens exprs with
                | (ExpressionUtilities.Identifier(idents, _))::appliedValues
                        when appliedValuesAreConstants appliedValues ->
                    
                    let funcName = String.Join(".", idents)
                    let funcStringParts = 
                        Seq.append
                            (Seq.singleton funcName)
                            (appliedValues 
                                |> Seq.take (appliedValues.Length - 1) 
                                |> Seq.choose (fun value -> ExpressionUtilities.tryFindTextOfRange value.Range fileContents))
                    let funcString = String.Join(" ", funcStringParts)
                    
                    match getLastElement appliedValues with
                    | SynExpr.Ident(lastArgument) when calledFunctionIdents.Length > 1 ->
                        if lastArgument.idText = lambdaArgument.idText then
                            funcString :: calledFunctionIdents
                        else
                            []
                    | SynExpr.App(_, false, _, _, _) as nextFunction ->
                        lambdaArgumentIsLastApplicationInFunctionCalls 
                            nextFunction 
                            lambdaArgument 
                            (funcString :: calledFunctionIdents)
                    | _ -> []
                | _ -> []
            | _ -> []

        match lambda.Arguments with
        | [singleParameter] ->
            match Helper.FunctionReimplementation.getLambdaParamIdent singleParameter with
            | Some paramIdent -> 
                match lambdaArgumentIsLastApplicationInFunctionCalls expression paramIdent [] with
                | [] -> None
                | funcStrings -> Some funcStrings
            | None -> None
        | _ -> None

    match tryReplaceWithFunctionComposition lambda.Body with
    | None -> Array.empty
    | Some funcStrings ->
        let suggestedFix =
            lazy(
                Some { FromRange = range; FromText = fileContents; ToText = String.Join(" >> ", funcStrings) })
        { Range = range
          Message = Resources.GetString("RulesCanBeReplacedWithComposition")
          SuggestedFix = Some suggestedFix
          TypeChecks = [] } |> Array.singleton

let runner (args:AstNodeRuleParams) =
    Helper.FunctionReimplementation.checkLambda args (validateLambdaCannotBeReplacedWithComposition args.FileContent)

let rule =
    { Name = "CanBeReplacedWithComposition"
      Identifier = Identifiers.CanBeReplacedWithComposition
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule