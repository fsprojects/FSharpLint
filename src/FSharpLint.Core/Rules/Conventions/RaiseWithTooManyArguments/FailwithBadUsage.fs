module FSharpLint.Rules.FailwithBadUsage

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System
open System.Collections.Generic

let mutable failwithErrorMessageList = Set.empty

type private BadUsageType =
    | EmptyMessage
    | DuplicateMessage
    | SwallowedException

let private runner (args: AstNodeRuleParams) =
    let generateError
        failwithKeyword
        failwithErrorMessage
        range
        (badUsageType: BadUsageType)
        (exceptionParam: Option<string>)
        =
        let suggestedFix =
            match exceptionParam with
            | Some param ->
                Some(
                    lazy
                        (Some
                            { FromText = sprintf "%s %s" failwithKeyword failwithErrorMessage
                              FromRange = range
                              ToText = sprintf "raise <| Exception(\"%s\", %s)" failwithErrorMessage param })
                )
            | _ -> None

        let message =
            match badUsageType with
            | EmptyMessage -> "Consider using non-empty error messages with failwith"
            | DuplicateMessage -> "Consider using unique error messages with failwith"
            | SwallowedException ->
                "failwith must not swallow exception details with it, rather use raise passing the current exception as innerException (2nd parameter of Exception constructor)"

        let error =
            { Range = range
              Message = String.Format(Resources.GetString "RulesFailwithBadUsage", message)
              SuggestedFix = suggestedFix
              TypeChecks = List.Empty }
            |> Array.singleton

        error

    let rec checkExpr node maybeIdentifier =
        match node with
        | SynExpr.App (_, _, SynExpr.Ident failwithId, expression, range) when
            failwithId.idText = "failwith"
            || failwithId.idText = "failwithf"
            ->
            match expression with
            | SynExpr.Const (SynConst.String (id, _, _), _) when id = "" ->
                generateError failwithId.idText id range BadUsageType.EmptyMessage maybeIdentifier
            | SynExpr.Const (SynConst.String (id, _, _), _) ->
                if Set.contains id failwithErrorMessageList then
                    generateError failwithId.idText id range BadUsageType.DuplicateMessage maybeIdentifier
                else
                    match maybeIdentifier with
                    | Some maybeId ->
                        generateError failwithId.idText id range BadUsageType.SwallowedException (Some maybeId)
                    | _ ->
                        failwithErrorMessageList <- failwithErrorMessageList.Add(id)
                        Array.empty
            | SynExpr.LongIdent (_, LongIdentWithDots (id, _), _, _) when
                (ExpressionUtilities.longIdentToString id) = "String.Empty"
                ->
                generateError
                    failwithId.idText
                    (ExpressionUtilities.longIdentToString id)
                    range
                    (BadUsageType.EmptyMessage)
                    (None)
            | _ -> Array.empty
        | SynExpr.TryWith (_, _, clauseList, _expression, _range, _, _) ->
            clauseList
            |> List.toArray
            |> Array.collect (fun clause ->
                match clause with
                | SynMatchClause (pat, _, app, _, _) ->
                    match pat with
                    | SynPat.Named (_, id, _, _, _) -> checkExpr app (Some id.idText)
                    | _ -> checkExpr app None)
        | _ -> Array.empty
        | _ -> Array.empty

    match args.AstNode with
    | AstNode.Expression expr -> checkExpr expr None
    | _ -> Array.empty

let cleanup () = failwithErrorMessageList <- Set.empty

let rule =
    { Name = "FailwithBadUsage"
      Identifier = Identifiers.FailwithBadUsage
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = cleanup } }
    |> AstNodeRule
