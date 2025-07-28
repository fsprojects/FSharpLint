module FSharpLint.Rules.ReimplementsFunction

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private validateLambdaIsNotPointless (text:string) lambda range =
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
            | ExpressionUtilities.Identifier(ident, _) -> Some(ident)
            | _ -> None

    let generateError (identifier:LongIdent) =
        let identifier =
            identifier
            |> List.map (fun ident ->
                if PrettyNaming.IsLogicalOpName ident.idText then
                    PrettyNaming.ConvertValLogicalNameToDisplayNameCore ident.idText |> sprintf "( %s )"
                else
                    ident.idText)
            |> String.concat "."

        let suggestedFix = lazy(
            ExpressionUtilities.tryFindTextOfRange range text
            |> Option.map (fun fromText -> { FromRange = range; ToText = identifier }))

        {
            Range = range
            Message = String.Format(Resources.GetString("RulesReimplementsFunction"), identifier)
            SuggestedFix = Some suggestedFix
            TypeChecks = List.Empty
        }

    let argumentsAsIdentifiers =
        lambda.Arguments
        |> List.map Helper.FunctionReimplementation.getLambdaParamIdent
        |> List.rev

    isFunctionPointless lambda.Body argumentsAsIdentifiers
    |> Option.map generateError
    |> Option.toArray

let runner (args:AstNodeRuleParams) =
    Helper.FunctionReimplementation.checkLambda args validateLambdaIsNotPointless

let rule =
    AstNodeRule
        {
            Name = "ReimplementsFunction"
            Identifier = Identifiers.ReimplementsFunction
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
